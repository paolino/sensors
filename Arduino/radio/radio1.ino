/*
 Copyright (C) 2015 <paolo.veronelli@mail.com>
*/
#include <Wire.h>
#include <ADXL345.h>
#include <L3G.h>
#include <SPI.h>
#include <nRF24L01.h>
#include <RF24.h>
//#include "printf.h"


RF24 radio(9,10);
ADXL345 acc;
L3G gyro;


const float alpha = 0.5;
float fXg = 0;
float fYg = 0;
float fZg = 0;

const uint64_t pipe = 0xF0F0F0F0E1LL;

int role = 0;

void setup(void)
{
 //Serial.begin(115200);
 
  acc.begin();
  gyro.init();
  delay(500);

  radio.begin();
  pinMode(3, OUTPUT); 
  gyro.enableDefault();
  radio.setRetries(15,15);
 

   if (role){
     radio.openReadingPipe(1,pipe);
     radio.startListening();
   }
  else {
      radio.openWritingPipe(pipe);
      radio.stopListening();
  }
}
 

 

void loop(void)
{


  if (role == 0)
  {

    float balance[6];
    double x=0,y=1,z=2;
    
    acc.read(&x, &y, &z);
    
    gyro.read();
    balance[3] = gyro.g.x;
    balance[4] = gyro.g.y;
    balance[5] = gyro.g.z;

    balance[0] = (float)x;
    balance[1] = (float)y;
    balance[2] = (float)z;
    
    radio.stopListening();
    digitalWrite(3,HIGH);    
    radio.write( balance, 24 );
    digitalWrite(3,LOW);    
 
  }

  if ( role == 1)
  {
    if ( radio.available() )
    {    
      float balance[6];
      digitalWrite(3,HIGH);
      radio.read( balance, 24 );
      Serial.print("[");
      for (int i=0;i<6;i++){
        Serial.print(balance[i]);
        if(i<5)Serial.print(",");
      }
      Serial.println("]");
      digitalWrite(3,LOW);
    }
  }
}

