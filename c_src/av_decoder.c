#include <erl_driver.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <sys/time.h>

enum {
    CMD_INIT = 1,
    CMD_ACTIVE_ONCE = 2
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
    char decoder_name[] = "h264";
    AVCodec *decoder = avcodec_find_decoder_by_name(decoder_name);
    // av_vaapi, av_vdpau
    
    if(!decoder) {
        fprintf(stderr, "Can't open decoder %s\r\n", decoder_name);
        return NULL;
    }

    AVCodecContext *dec = avcodec_alloc_context2(AVMEDIA_TYPE_VIDEO);
    dec->lowres = 0;   
    // dec->idct_algo = FF_IDCT_LIBMPEG2MMX;   
    // dec->flags2 |= CODEC_FLAG2_CHUNKS;   
    dec->debug |= FF_DEBUG_STARTCODE;
    dec->skip_frame = AVDISCARD_DEFAULT;   
    dec->skip_idct = AVDISCARD_DEFAULT;   
    dec->skip_loop_filter = AVDISCARD_DEFAULT;
    // dec->error_resilience = FF_ER_CAREFUL;   
    // dec->error_concealment = 3;  
    
    dec->extradata_size = decoder_config_len;
    dec->extradata = (uint8_t *)malloc(dec->extradata_size);
    memcpy(dec->extradata, (const char *)decoder_config, dec->extradata_size);

    // fprintf(stderr, "Got %u bytes of config: <<", dec->extradata_size);
    // 
    // uint32_t i;
    // for(i = 0; i < dec->extradata_size; i++) {
    //  if(i) fprintf(stderr, ",");
    //  fprintf(stderr, "%d", ((unsigned char*)dec->extradata)[i]);
    // }
    // fprintf(stderr, ">>\r\n");
    if(avcodec_open(dec, decoder) < 0) {
        free(dec->extradata);
        dec->extradata = NULL;
        av_free(dec);
        fprintf(stderr, "Can't open decoder\r\n");
        return NULL;
    }
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
    break;
    default:
    return 0;
  }
  return 0;
}


static void av_async_decode(void *async_data) {
    struct timeval tv1;
    gettimeofday(&tv1, NULL);
    H264Frame *frame = (H264Frame *)async_data;
    
    H264Decoder *handle = frame->decoder;
    
    int frame_ready = 0, len;
    AVFrame *decoded;

    decoded = avcodec_alloc_frame();
    AVPacket avpkt;
    avpkt.data = frame->h264->orig_bytes;
    avpkt.size = frame->h264->orig_size;
    // uint32_t len1 = ntohl(*(uint32_t *)frame->data);
    // fprintf(stderr, "Still decoding %p %u, %d: <<", avpkt.data, len1, avpkt.size);
    // int i;
    // for( i = 0; i< 10; i++) fprintf(stderr, "%s%d", i == 0 ? "" : ",", frame->data[i]);
    // fprintf(stderr, ">>\r\n");
    len = avcodec_decode_video2(handle->dec, decoded, &frame_ready, &avpkt);
    
    if(len == -1) {
        // fprintf(stderr, "Failed to decode\r\n");
    } else if(len != frame->h264->orig_size) {
        // fprintf(stderr, "Consumed not all: %d/%d\r\n", len, frame->h264->orig_size);
    }
    
    if(frame_ready) {
        int width = handle->dec->width;
        int height = handle->dec->height;
            
        if(!handle->scale_ctx) {
            // fprintf(stderr, "Started stream from camera %dx%d\r\n", width, height);
            handle->scale_ctx = sws_getContext(
              width, height, PIX_FMT_YUV420P, 
              width, height, PIX_FMT_YUV420P, 
              SWS_FAST_BILINEAR, NULL, NULL, NULL
            );
        }
        
        
        
        int stride_size = width*height;
        frame->yuv = driver_alloc_binary(stride_size*3/2);
        
        uint8_t *yuv_data = (uint8_t *)frame->yuv->orig_bytes;
        
        uint8_t *plane[4] = {yuv_data, yuv_data+stride_size, yuv_data+stride_size+stride_size/4, NULL};
        int stride[4] = {width, width/2, width/2, 0};
        
        
        // for(i = 0; i < handle->dec->height; i++) {
        //     memcpy(y.data + handle->dec->width*i, decoded->data[0] + decoded->linesize[0]*i, handle->dec->width);
        // }
        // for(i = 0; i < handle->dec->height/2; i++) {
        //     memcpy(u.data + handle->dec->width/2*i, decoded->data[1] + decoded->linesize[1]*i, handle->dec->width/2);
        //     memcpy(v.data + handle->dec->width/2*i, decoded->data[2] + decoded->linesize[2]*i, handle->dec->width/2);
        // }
        sws_scale(handle->scale_ctx, (const uint8_t **)decoded->data, decoded->linesize, 0, height, plane, stride);
        // This strange copy-scale is required to get planar yuv array.

        struct timeval tv2;
        gettimeofday(&tv2, NULL);
        handle->total_time += (tv2.tv_sec * 1000 + tv2.tv_usec / 1000) - (tv1.tv_sec * 1000 + tv1.tv_usec / 1000);
    }
    // fprintf(stderr, "Frame %d -> %d (%d, %d)\r\n", frame->h264->orig_size, frame_ready, driver_binary_get_refc(frame->h264), driver_binary_get_refc(frame->yuv));
    driver_free_binary(frame->h264);
    av_free(decoded);
}



static void av_decoder_schedule_decode(ErlDrvData drv_data, ErlIOVec *ev)
{
  H264Decoder* d = (H264Decoder*)drv_data;
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
    ErlDrvTermData reply[] = {
      ERL_DRV_ATOM, driver_mk_atom("yuv"),
      ERL_DRV_PORT, driver_mk_port(decoder->port),
      ERL_DRV_BINARY, (ErlDrvTermData)frame->yuv, (ErlDrvTermData)frame->yuv->orig_size, 0,
      ERL_DRV_TUPLE, 3
    };
    driver_output_term(decoder->port, reply, sizeof(reply) / sizeof(reply[0]));
    driver_free_binary(frame->yuv);
  } else {
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
