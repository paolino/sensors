/*
 Copyright (C) 2015 <paolo.veronelli@mail.com>
*/
#include <Wire.h>
#include <ADXL345.h>
#include <L3G.h>
#include <SPI.h>
#include <nRF24L01.h>
#include <RF24.h>
#include <HMC5883L.h>

//#include "printf.h"


RF24 radio(9,10);
ADXL345 acc;
L3G gyro;


const float alpha = 0.5;
float fXg = 0;
float fYg = 0;
float fZg = 0;
HMC5883L compass;
const uint64_t pipe = 0xF0F0F0F0E1LL;

int role = 0;
int channel=30;
//int channel = 50;


void setup(void)
{

   radio.begin();
   radio.setRetries(15,15);
   radio.setChannel(channel);
   if (role){
     Serial.begin(115200);
     radio.openReadingPipe(1,pipe);
     radio.startListening();
   }
  else {  
      acc.begin();
      gyro.init();
      gyro.enableDefault();
      compass.begin();
      compass.setDataRate(HMC5883L_DATARATE_75HZ);
      radio.openWritingPipe(pipe);
      radio.stopListening();
  }

}
 

 

void loop(void)
{


  if (role == 0)
  {

    uint16_t balance[9];
    //double x=0,y=1,z=2;
    Vector raw = compass.readRaw();
    acc.read(&balance[6], &balance[7], &balance[8]);

    gyro.read();
    balance[3] = gyro.g.x;
    balance[4] = gyro.g.y;
    balance[5] = gyro.g.z;

    balance[0] = raw.XAxis;
    balance[1] = raw.YAxis;
    balance[2] = raw.ZAxis;
    radio.stopListening();
    
    radio.write( balance, 18);
 
 
  }

  if ( role == 1)
  {
    if ( radio.available() )
    {    
      int16_t balance[6];
     
      radio.read( balance, 18);
      Serial.print("[");
      for (int i=0;i<9;i++){
        Serial.print(balance[i]);
        if(i<8)Serial.print(",");
      }
      Serial.println("]");
      
    }
  }
}

