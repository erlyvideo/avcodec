#include <erl_driver.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <sys/time.h>


enum {
    CMD_INIT = 1,
    CMD_INFO = 2
    };

typedef struct {
  ErlDrvPort port;
  ErlDrvTermData owner_pid;
  AVCodecContext *dec;
  struct SwsContext* scale_ctx;	
  uint32_t total_time;
  unsigned int key;
} H264Decoder;

typedef struct {
  H264Decoder *decoder;
  ErlDrvBinary *h264;
  ErlDrvBinary *yuv;
  ErlDrvBinary *sample;
} H264Frame;

static unsigned int av_decoder_key = 0;

static int av_decoder_init()
{
  avcodec_init();
  avcodec_register_all();
  av_log_set_level(AV_LOG_VERBOSE);
  return 0;
}

AVCodecContext *new_software_decoder(uint8_t *decoder_config, uint32_t decoder_config_len)
{
    char decoder_name[10];
    int config_size,i;
    AVCodecContext *dec;
    bzero(decoder_name, sizeof(decoder_name));
    memcpy(decoder_name, decoder_config, 4);
    decoder_config += 4;
    decoder_config_len -= 4;

    // av_vaapi, av_vdpau
    if (!strcmp(decoder_name,"wmv3")){
      dec = avcodec_alloc_context2(AVMEDIA_TYPE_VIDEO);
      memcpy(&config_size,decoder_config,sizeof(config_size));
      decoder_config += sizeof(config_size);
      decoder_config_len -=sizeof(config_size);  
      memcpy(&dec->width,decoder_config,sizeof(dec->width));   
      memcpy(&dec->height,decoder_config+sizeof(dec->width),sizeof(dec->height));
      decoder_config += config_size;
      decoder_config_len -= config_size;
    } else if(!strcmp(decoder_name,"wmav")){
      dec = avcodec_alloc_context2(AVMEDIA_TYPE_AUDIO);
      bzero(decoder_name, sizeof(decoder_name));
      memcpy(decoder_name,decoder_config-4,5);
      decoder_config++; decoder_config_len--;
      memcpy(&dec->channels,decoder_config,sizeof(dec->channels));
      memcpy(&dec->bit_rate,decoder_config+=sizeof(dec->channels),sizeof(dec->bit_rate));
      memcpy(&dec->sample_rate,decoder_config+=sizeof(dec->bit_rate),sizeof(dec->sample_rate));
      decoder_config+=sizeof(dec->sample_rate);decoder_config_len-=sizeof(dec->sample_rate) + sizeof(dec->bit_rate) + sizeof(dec->channels);
    };
    printf("PARAMS: %i %i %i",dec->bit_rate,dec->sample_rate,dec->channels);
    AVCodec *decoder = avcodec_find_decoder_by_name(decoder_name);
    if(!decoder) {
      fprintf(stderr, "Can't find decoder %s\r\n", decoder_name);
      return NULL;
    };
    dec->extradata_size = decoder_config_len;
    dec->extradata = av_mallocz(dec->extradata_size);
    memcpy(dec->extradata, (const char *)decoder_config, dec->extradata_size);
    av_log(dec,AV_LOG_ERROR,"Payload: \n");
    for(i=0;i<dec->extradata_size;i++)
      av_log(dec,AV_LOG_ERROR,"%i, ",(int)dec->extradata[i]);
    if(avcodec_open(dec, decoder) < 0) {
        free(dec->extradata);
        dec->extradata = NULL;
        av_free(dec);
        fprintf(stderr, "Can't open decoder %i\r\n");
        return NULL;
    };
    return dec;    
}





static ErlDrvData av_decoder_drv_start(ErlDrvPort port, char *buff)
{
    H264Decoder* d = (H264Decoder *)driver_alloc(sizeof(H264Decoder));
    bzero(d, sizeof(H264Decoder));
    d->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    d->owner_pid = driver_caller(port);
    d->key = av_decoder_key++;
    return (ErlDrvData)d;
}


static void av_decoder_drv_stop(ErlDrvData handle)
{
  // H264Decoder* d = (H264Decoder *)handle;
  driver_free((char*)handle);
}


static void av_exit(H264Decoder *d)
{
  ErlDrvTermData reply[] = {
    ERL_DRV_ATOM, driver_mk_atom("av_decoder_closed"),
    ERL_DRV_PORT, driver_mk_port(d->port),
    ERL_DRV_TUPLE, 2
  };
  driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
  driver_exit(d->port, 0);
}



static int av_decoder_drv_command(ErlDrvData handle, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen) {
  H264Decoder* d = (H264Decoder*) handle;
  
  switch(command) {
    case CMD_INIT: {
      d->dec = new_software_decoder((uint8_t *)buf, len);
      if(!d->dec) {
        driver_failure_atom(d->port, "cant_open_decoder");
        return 0;
      }
      memcpy(*rbuf, "ok", 2);
      return 2;
    }    
    case CMD_INFO: {
      uint32_t *out = (uint32_t *)*rbuf;
      out[0] = d->dec ? htonl(d->dec->width) : 0;
      out[1] = d->dec ? htonl(d->dec->height) : 0;
      out[2] = htonl(d->total_time);
      return 3*4;
    }
    break;
    default:
    return 0;
  }
  return 0;
}

static void audio_async_decode(void *async_data){
  H264Frame *frame = (H264Frame *)async_data;
  H264Decoder *handle = frame->decoder;
  int16_t *outbuf=NULL;
  int len;
  int size_out;
  outbuf=av_malloc(AVCODEC_MAX_AUDIO_FRAME_SIZE*2);
  AVPacket avpkt;  
  avpkt.data = (uint8_t *)frame->h264->orig_bytes;
  avpkt.size = frame->h264->orig_size;
  size_out = AVCODEC_MAX_AUDIO_FRAME_SIZE*2;
  len = avcodec_decode_audio3(handle->dec, outbuf, &size_out,&avpkt);  
  if(len == -1) {
    av_free(outbuf);
    return;
  };
  frame->sample = driver_alloc_binary(size_out);    
  memcpy(frame->sample->orig_bytes,outbuf,size_out);
  frame->sample->orig_size = size_out; 
  av_free(outbuf);
  driver_free_binary(frame->h264);
}

static void video_async_decode(void *async_data){
  H264Frame *frame = (H264Frame *)async_data;
  H264Decoder *handle = frame->decoder;
  AVCodecContext *avctx = handle->dec;
  AVCodec *codec = avctx->codec;
  AVPacket avpkt;
  av_init_packet(&avpkt);
  struct timeval tv1;
  struct timeval tv2;
  gettimeofday(&tv1, NULL);
  int frame_ready = 0, len,i;
  AVFrame *decoded;
  decoded = avcodec_alloc_frame();
  frame_ready = sizeof(AVFrame)*2;
  avpkt.data = (uint8_t *)frame->h264->orig_bytes;
  avpkt.size = frame->h264->orig_size;
  len = avcodec_decode_video2(handle->dec, decoded, &frame_ready, &avpkt);  
  //  av_log(avctx,AV_LOG_ERROR,"\nORIGSIZE: %i; LEN %i \n",avpkt.size,len);
  if(len == -1) {
    av_free(decoded);
    return;
  };
  if(frame_ready) {
    int width = handle->dec->width;
    int height = handle->dec->height;
      if(handle->scale_ctx==NULL) {
        fprintf(stderr, "Started stream from camera %dx%d\r\n", width, height);
        handle->scale_ctx = sws_getContext(
          width, height, avctx->pix_fmt,
          width, height, avctx->pix_fmt,
	  SWS_FAST_BILINEAR, 
          NULL, NULL, NULL
        );
      }        
    int stride_size = width*height;
    frame->yuv = driver_alloc_binary(stride_size*3/2);
    uint8_t *yuv_data = (uint8_t *)frame->yuv->orig_bytes;        
    uint8_t *plane[4] = {yuv_data, yuv_data+stride_size, yuv_data+stride_size+stride_size/4, NULL};
    int stride[4] = {width, width/2, width/2, 0};
    sws_scale(handle->scale_ctx, (const uint8_t **)decoded->data, decoded->linesize, 0, height, plane, stride);
  }
  gettimeofday(&tv2, NULL);
  handle->total_time += (tv2.tv_sec * 1000 + tv2.tv_usec / 1000) - (tv1.tv_sec * 1000 + tv1.tv_usec / 1000);
  av_free(decoded);
  driver_free_binary(frame->h264);
}


static void av_async_decode(void *async_data) {
    H264Frame *frame = (H264Frame *)async_data;
    H264Decoder *handle = frame->decoder;
    AVCodecContext *avctx = handle->dec;
    AVCodec *codec = avctx->codec;
    /*    av_log(avctx,AV_LOG_ERROR,"%i",avctx->codec_tag);
    av_log(avctx,AV_LOG_ERROR,"%i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; ",
	   avctx->bit_rate,
	   avctx->bit_rate_tolerance,
	   avctx->flags,
	   avctx->sub_id,
	   avctx->me_method,
	   avctx->extradata_size,
	   avctx->width,
	   avctx->height,
	   avctx->gop_size,
	   avctx->sample_rate,
	   avctx->channels,
	   avctx->frame_size,
	   avctx->delay,
	   avctx->qmin,
	   avctx->max_qdiff,
	   avctx->max_b_frames,
	   avctx->rc_strategy,
	   avctx->b_frame_strategy,
	   avctx->mv_bits,
	   avctx->header_bits,
	   avctx->i_tex_bits,
	   avctx->p_tex_bits,
	   avctx->i_count,
	   avctx->p_count,
	   avctx->skip_count,
	   avctx->frame_bits,
	   avctx->codec_type,
	   avctx->codec_id,
	   avctx->codec_tag,
	   avctx->workaround_bugs,
	   avctx->luma_elim_threshold,
	   avctx->chroma_elim_threshold,
	   avctx->strict_std_compliance,
	   avctx->b_quant_offset,
	   avctx->error_recognition,
	   avctx->has_b_frames,
	   avctx->block_align,
	   avctx->parse_only,
	   avctx->mpeg_quant,
	   avctx->stats_out,
	   avctx->rc_qmod_freq,
	   avctx->rc_override_count,
	   avctx->rc_max_rate,
	   avctx->rc_min_rate,
	   avctx->rc_buffer_size,
	   avctx->dct_algo
);
    av_log(avctx,AV_LOG_ERROR,"\nADLOG::%i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; %i; \n",
	   avctx->idct_algo,
	   avctx->slice_count,
	   avctx->slice_offset,
	   avctx->error_concealment,
	   avctx->dsp_mask,
	   avctx->bits_per_coded_sample,
	   avctx->prediction_method,
	   avctx->debug,
	   avctx->debug_mv,
	   avctx->me_cmp,
	   avctx->me_sub_cmp,
	   avctx->mb_cmp,
	   avctx->ildct_cmp,
	   avctx->dia_size,
	   avctx->last_predictor_count,
	   avctx->pre_me,
	   avctx->me_pre_cmp,
	   avctx->pre_dia_size,
	   avctx->me_subpel_quality,
	   avctx->dtg_active_format,
	   avctx->me_range,
	   avctx->intra_quant_bias,
	   avctx->inter_quant_bias,
	   avctx->color_table_id,
	   avctx->internal_buffer_count,
	   avctx->internal_buffer,
	   avctx->global_quality,
	   avctx->coder_type,
	   avctx->context_model,
	   avctx->slice_flags,
	   avctx->xvmc_acceleration,
	   avctx->mb_decision,
	   avctx->stream_codec_tag,
	   avctx->scenechange_threshold,
	   avctx->lmin,
	   avctx->lmax
	   );*/
    if(codec->type == AVMEDIA_TYPE_AUDIO){
      audio_async_decode(async_data);
    }
    else if (codec->type == AVMEDIA_TYPE_VIDEO){
      video_async_decode(async_data);
    }
    else 
      return;
}



static void av_decoder_schedule_decode(ErlDrvData drv_data, ErlIOVec *ev)
{
  H264Decoder* d = (H264Decoder*)drv_data;
  //  av_log(d->dec,AV_LOG_WARNING,"\nVSIZE:: %i\n");
  ErlDrvBinary *h264 = NULL;
  int i;
  for(i = 0; i < ev->vsize; i++) {
    if(h264 && ev->binv[i]) {
      driver_failure_atom(d->port, "invalid_output_vector");
      return;
    }
    if(ev->binv[i]) h264 = ev->binv[i];
  }
  if(!h264) {
    driver_failure_atom(d->port, "invalid_output_vector");
    return;
  }
  H264Frame *frame = driver_alloc(sizeof(H264Frame));
  bzero(frame, sizeof(H264Frame));
  frame->h264 = h264;
  // av_log(NULL,AV_LOG_INFO,"Size:: %i ",h264->orig_size);
  frame->decoder = d;
  driver_binary_inc_refc(h264);
  // I must change driver_free for other, more clever clearer, because yuv field must be also freed.
  driver_async(d->port, &d->key, av_async_decode, frame, driver_free);
}


static void av_decoder_decoded(ErlDrvData drv_data, ErlDrvThreadData thread_data)
{
  H264Decoder *decoder = (H264Decoder *)drv_data;
  H264Frame *frame = (H264Frame *)thread_data;
  
  // fprintf(stderr, "Decoding finished: %p %ld\r\n", frame->yuv, frame->yuv ? frame->yuv->orig_size : -1);
  
  if(frame->yuv) {
    //    av_log(NULL,AV_LOG_WARNING,"\nPORTyuv:: %i\n",decoder->port);
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("yuv"),
      ERL_DRV_PORT, driver_mk_port(decoder->port),
      ERL_DRV_BINARY, (ErlDrvTermData)frame->yuv, (ErlDrvTermData)frame->yuv->orig_size, 0,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(decoder->port, reply, sizeof(reply) / sizeof(reply[0]));
    driver_free_binary(frame->yuv);
  } else if(frame->sample){
    //    av_log(NULL,AV_LOG_WARNING,"\nPORTsample:: %i\n",decoder->port);
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("sample"),
      ERL_DRV_PORT, driver_mk_port(decoder->port),
      ERL_DRV_BINARY, (ErlDrvTermData)frame->sample, (ErlDrvTermData)frame->sample->orig_size, 0,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(decoder->port, reply, sizeof(reply) / sizeof(reply[0]));
    driver_free_binary(frame->sample);
  } else {
    //    av_log(decoder->dec,AV_LOG_ERROR,"NOT YUV");
    av_log(NULL,AV_LOG_WARNING,"\nPORTelse:: %i\n",decoder->port);
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("yuv"),
      ERL_DRV_PORT, driver_mk_port(decoder->port),
      ERL_DRV_TUPLE, 2
    };
    driver_output_term(decoder->port, reply, sizeof(reply) / sizeof(reply[0]));
  }
  driver_free(frame);
}



ErlDrvEntry av_decoder_driver_entry = {
    av_decoder_init,			/* F_PTR init, N/A */
    av_decoder_drv_start,		/* L_PTR start, called when port is opened */
    av_decoder_drv_stop,		/* F_PTR stop, called when port is closed */
    NULL,	    /* F_PTR output, called when erlang has sent */
    NULL,		                  /* F_PTR ready_input, called when input descriptor ready */
    NULL,	                    /* F_PTR ready_output, called when output descriptor ready */
    "av_decoder_drv",		/* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,     /* void *handle */
    av_decoder_drv_command,			/* F_PTR control, port_command callback */
    NULL,			                 /* F_PTR timeout, reserved */
    av_decoder_schedule_decode,	                     /* F_PTR outputv, reserved */
    av_decoder_decoded,       /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,     /* ERL_DRV_FLAGs */
    NULL,     /* void *handle2 */
    NULL,     /* process_exit */
    NULL      /* stop_select */
};
DRIVER_INIT(av_decoder_drv) /* must match name in driver_entry */
{
    return &av_decoder_driver_entry;
}
