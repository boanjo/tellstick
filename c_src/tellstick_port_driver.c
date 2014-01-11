/* tellstick_port_driver.c */

#include <telldus-core.h>
#include <time.h>
#include <stdio.h>
#include "erl_driver.h"
#include <unistd.h>
#include "ei.h"    

#define DATA_LENGTH 20

int callbackId = 0;
int callbackDeviceId = 1;
int callbackDeviceChangeId = 2;
typedef struct {
  ErlDrvPort port;
} tellstick_data;

tellstick_data* handlePtr;
typedef ErlDrvTermData *ErlDriverTerm;


static void update_device_name() 
{
  int intNumberOfDevices = tdGetNumberOfDevices();
  int i = 0;
  
  for (i = 0; i < intNumberOfDevices; i++) {
    int id = tdGetDeviceId( i );
    char *name = tdGetName( id );
    
    ErlDriverTerm spec[] = {
      ERL_DRV_ATOM, driver_mk_atom("device_name"),
      ERL_DRV_INT, id,
      ERL_DRV_INT, 20,
      ERL_DRV_STRING, (ErlDrvTermData)name, strlen(name),
      ERL_DRV_TUPLE, 4,
    };
    erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   

    // printf("%d\t%s\n", id, name);
    tdReleaseString(name);

  }
}

static void update_device_state() 
{
  int intNumberOfDevices = tdGetNumberOfDevices();
  int i = 0;
  
  for (i = 0; i < intNumberOfDevices; i++) {
    int id = tdGetDeviceId( i );    
    char* dim_value = "-1";
    int state = tdLastSentCommand( id, TELLSTICK_TURNON | TELLSTICK_TURNOFF);    
    ErlDriverTerm spec[] = {
      ERL_DRV_ATOM, driver_mk_atom("device_event"),
      ERL_DRV_INT, id,
      ERL_DRV_INT, state,
      ERL_DRV_STRING, (ErlDriverTerm)"-1", strlen(dim_value),
      ERL_DRV_TUPLE, 4,
    };
    erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
  }
}

void WINAPI sensorEvent(const char *protocol, const char *model, int sensorId, int dataType, const char *value, int ts, int callbackId, void *context) {

  ErlDriverTerm spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("sensor_event"),
    ERL_DRV_INT, sensorId,
    ERL_DRV_INT, dataType,
    ERL_DRV_STRING, (ErlDriverTerm)value, 10,
    ERL_DRV_TUPLE, 4,
  };
  erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
}


void WINAPI deviceEvent(int deviceId, int method, const char *data, int callbackId, void *context) {
  ErlDriverTerm spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("device_event"),
    ERL_DRV_INT, deviceId,
    ERL_DRV_INT, method,
    ERL_DRV_STRING, (ErlDriverTerm)data, 10,
    ERL_DRV_TUPLE, 4,
  };
  erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
}

void WINAPI deviceChange(int deviceId, int method, const char *data, int callbackId, void *context) {
  ErlDriverTerm spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("device_change"),
    ERL_DRV_INT, deviceId,
    ERL_DRV_INT, method,
    ERL_DRV_STRING, (ErlDriverTerm)data, 10,
    ERL_DRV_TUPLE, 4,
  };
  erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
}


static ErlDrvData tellstick_drv_start(ErlDrvPort port, char *buff)
{
  handlePtr= (tellstick_data*)driver_alloc(sizeof(tellstick_data));
  handlePtr->port = port;

  tdInit();
  //Register for callback
  callbackId= tdRegisterSensorEvent( (TDSensorEvent)&sensorEvent, 0 );
  callbackDeviceId = tdRegisterDeviceEvent( (TDDeviceEvent)&deviceEvent, 0 );
  callbackDeviceChangeId = tdRegisterDeviceChangeEvent( (TDDeviceChangeEvent)&deviceChange, 0 );
  
  update_device_name();
  update_device_state();
  
  return (ErlDrvData)handlePtr;
}



static void tellstick_drv_stop(ErlDrvData handle)
{

  printf("\n\n DOWN!!!!!!!\n\n\n");

  //Cleanup
  tdUnregisterCallback( callbackId );  
  tdUnregisterCallback( callbackDeviceId );  
  tdUnregisterCallback( callbackDeviceChangeId );  
  tdClose();

  driver_free((char*)handle);
}


static void tellstick_drv_output(ErlDrvData handle, char *buff, 
			       ErlDrvSizeT bufflen)
{
  char fn = buff[0], res;
  int deviceId = (int)buff[1];
  //printf("Got %d and %d\n", fn, deviceId);
  int retVal = 0;
  
  if (fn == 1) {
    retVal = tdTurnOn( deviceId );
  } else if (fn == 2) {
    retVal = tdTurnOff( deviceId );
  } else if( fn == 3) {
    update_device_name();
  } else if( fn == 4) {
    update_device_state();
  } else if(fn == 5) {
    char protocol[DATA_LENGTH], model[DATA_LENGTH];
    int sensorId = 0, dataTypes = 0;
    while(tdSensor(protocol, DATA_LENGTH, model, DATA_LENGTH, &sensorId, &dataTypes) == TELLSTICK_SUCCESS) {
      //Print the sensor
      // printf("%s,\t%s,\t%i\n", protocol, model, sensorId);

      char value[DATA_LENGTH];
      char timeBuf[80];
      time_t timestamp = 0;
      if (dataTypes & TELLSTICK_TEMPERATURE) {
	tdSensorValue(protocol, model, sensorId, TELLSTICK_TEMPERATURE, value, DATA_LENGTH, (int *)&timestamp);
	ErlDriverTerm spec[] = {
	  ERL_DRV_ATOM, driver_mk_atom("sensor_event"),
	  ERL_DRV_INT, sensorId,
	  ERL_DRV_INT, 1,
	  ERL_DRV_STRING, (ErlDriverTerm)value, 10,
	  ERL_DRV_TUPLE, 4,
	};
	erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
	
      }
      if (dataTypes & TELLSTICK_HUMIDITY) {
	tdSensorValue(protocol, model, sensorId, TELLSTICK_TEMPERATURE, value, DATA_LENGTH, (int *)&timestamp);
	ErlDriverTerm spec[] = {
	  ERL_DRV_ATOM, driver_mk_atom("sensor_event"),
	  ERL_DRV_INT, sensorId,
	  ERL_DRV_INT, 2,
	  ERL_DRV_STRING, (ErlDriverTerm)value, 10,
	  ERL_DRV_TUPLE, 4,
	};
	erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
	
      }
      
    }
  } else if(fn == 6) {
    char* name =  tdGetName( deviceId );
    tdReleaseString(name);
  } 

  ErlDriverTerm spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("output_conf"),
    ERL_DRV_INT, retVal,
    ERL_DRV_TUPLE, 2,
  };
  erl_drv_output_term(driver_mk_port(handlePtr->port), spec, sizeof(spec) / sizeof(spec[0]));   
}


ErlDrvEntry tellstick_driver_entry = {
  NULL,/* F_PTR init, called when driver is loaded */
  tellstick_drv_start,/* L_PTR start, called when port is opened */
  tellstick_drv_stop,/* F_PTR stop, called when port is closed */
  tellstick_drv_output,/* F_PTR output, called when erlang has sent */
  NULL,/* F_PTR ready_input, called when input descriptor ready */
  NULL,/* F_PTR ready_output, called when output descriptor ready */
  "tellstick_drv",/* char *driver_name, the argument to open_port */
  NULL,/* F_PTR finish, called when unloaded */
  NULL,                       /* void *handle, Reserved by VM */
  NULL,/* F_PTR control, port_command callback */
  NULL,/* F_PTR timeout, reserved */
  NULL,/* F_PTR outputv, reserved */
  NULL,                       /* F_PTR ready_async, only for async drivers */
  NULL,                       /* F_PTR flush, called when port is about 
				    to be closed, but there is data in driver 
				    queue */
  NULL,                       /* F_PTR call, much like control, sync call
				 to driver */
  NULL,                       /* F_PTR event, called when an event selected 
				 by driver_event() occurs. */
  ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				 set to indicate driver versioning */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				     set to this value */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				     set to this value */
  0,                          /* int driver_flags, see documentation */
  NULL,                       /* void *handle2, reserved for VM use */
  NULL,                       /* F_PTR process_exit, called when a 
				 monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an 
				   event object */
};

DRIVER_INIT(tellstick_drv) /* must match name in driver_entry */
{
  return &tellstick_driver_entry;
}
