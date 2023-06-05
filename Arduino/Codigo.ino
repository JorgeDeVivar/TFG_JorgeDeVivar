//////////////////////////////////////////////////////////////////////////LIBRERIAS/////////////////////////////////////////////////////////////////////////////
#include <MHZ.h>
#include <SPI.h>
#include <LiquidCrystal.h>
#include <LiquidCrystal_I2C.h>
#include <Wire.h>
#include <TimeLib.h>
#include <Time.h>
#include <DHT.h>
#include <DHT_U.h>
#include <SoftwareSerial.h>
#include <MHZ19.h>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////DEFINIR VALORES//////////////////////////////////////////////////////////////////////////

//Declaramos los valores para obtener la temperatura y la humedad del DHT22
#define DHTPIN 5 //Pin 5 del Arduino Mega
#define DHTTYPE DHT22 //Definir el sensor como un DHT22
DHT dht(DHTPIN, DHTTYPE);
float temperatura;
float humedad;
// Declaramos la variable del tipo time_t
time_t fecha;
//Crear el objeto lcd  dirección  0x3F y 16 columnas x 2 filas
LiquidCrystal_I2C lcd(0x27,16,2);
//Detectar los LEDs
const int ledPIN1 = 2;
const int ledPIN2 = 3;
const int ledPIN3 = 4;
//Detectar el sensor MH-Z19B para obtener el CO2
int DataPin = 8;
int ppmrange = 5000;
unsigned long pwmtime;
int PPM = 0;
float pulsepercent=0;
//Detectar el sensor MQ7 para obtener el CO
float RS_gas = 0;
float ratio = 0;
float sensorValue = 0;
float sensor_volt = 0;
float R0 = 9400.0;
//Detectar el sensor de movimiento PIR
#define pirPin 9
int calibrationTime = 30;
long unsigned int lowIn;
long unsigned int pause = 5000;
boolean lockLow = true;
boolean takeLowTime;
int PIRValue = 0;
//Botón rojo
int val = 0; //val se emplea para almacenar el estado del boton
int state = 1; // 0 LED apagado, mientras que 1 encendido
int old_val = 0; // almacena el antiguo valor de val


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////CODIGO ESTATICO//////////////////////////////////////////////////////////////////////////////

void setup()
{
  
  Serial.begin(9600);
  dht.begin();
  //Establecemos la hora y la fecha 
  setTime(10, 31, 0, 22, 2, 2022);
  // Inicializar el LCD
  lcd.init();
  //Encender la luz de fondo.
  lcd.backlight();
  //Salidas de LEDs
  pinMode(ledPIN1 , OUTPUT); 
  pinMode(ledPIN2 , OUTPUT); 
  pinMode(ledPIN3 , OUTPUT); 
  //Iniciando comunicación serial con el sensor DH-Z19B
  pinMode(DataPin, INPUT);
  //Iniciando la comunicación con el sensor PIR

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////CODIGO EN BUCLE/////////////////////////////////////////////////////////////////////////////////////

void loop()
{
    float h = dht.readHumidity(); //Leemos la Humedad
    float t = dht.readTemperature(); //Leemos la temperatura en grados Celsius
    float f = dht.readTemperature(true); //Leemos la temperatura en grados Fahrenheit
    // Obtenemos la fecha actual
    fecha = now();
    //Obtener CO2 en ppm
    pwmtime = pulseIn(DataPin, HIGH, 2000000) / 1000;
    float pulsepercent = pwmtime / 1004.0;
    PPM = ppmrange * pulsepercent;
    //Obtener CO en ppm
    sensorValue = analogRead(A2);
    sensor_volt = sensorValue/1024*5.0;
    RS_gas = (5.0-sensor_volt)/sensor_volt;
    ratio = RS_gas/R0; //Replace R0 with the value found using the sketch above
    float x = 1538.46 * ratio;
    float ppm = pow(x,-1.709);
    //Botón Rojo
    val = analogRead(A0);
    
    // Imprimimos la hora
    //Serial.print("Hora: ");
    Serial.print(hour(fecha));
    Serial.print(":");
    Serial.print(minute(fecha));
    Serial.print(":");
    Serial.print(second(fecha));
    Serial.print(",");
    //Serial.print("Fecha: ");
    Serial.print(day(fecha));
    Serial.print("/");
    Serial.print(month(fecha));
    Serial.print("/");
    Serial.print(year(fecha));
    Serial.print(",");
    // Imprimos datos de sensor DHT22
    //Serial.print("Humedad ");
    Serial.print(h);
    Serial.print(",");
    //Serial.print("Temperatura: ");
    Serial.print(t);
    Serial.print(",");
    //Imprimimos el MH-Z19B
    //Serial.print("CO2 en ppm: ");
    Serial.print(PPM);
    Serial.print(",");
    //Imprimimos el MQ7
    //Serial.print("CO en ppm: ");
    Serial.println(ppm);

    
    //Display
    //lcd.init();
    //lcd.backlight();
    lcd.display();
    lcd.clear();
    lcd.setCursor(0,0);
    //lcd.print("Humedad = ");
    lcd.print(h);
    lcd.print("%    ");
    //lcd.print("Temper. = ");
    lcd.print(t);
    lcd.print(" C");
    lcd.setCursor(0,1);
    lcd.print("CO2 = ");
    lcd.print(PPM);
    lcd.print(" ppm");
    lcd.setCursor(0,2);
    lcd.print("CO = ");
    lcd.print(ppm);
    lcd.print(" ppm");

    //LEDs para ver la calidad del aire
    if (PPM <= 800){
      digitalWrite(ledPIN1,HIGH); 
      digitalWrite(ledPIN2,LOW);
      digitalWrite(ledPIN3,LOW);
    }
    if (PPM > 800 && PPM <= 1000){
      digitalWrite(ledPIN1,LOW);
      digitalWrite(ledPIN2,HIGH);
      digitalWrite(ledPIN3,LOW);
    }
    if (PPM > 1000){
      digitalWrite(ledPIN1,LOW);
      digitalWrite(ledPIN2,LOW);
      digitalWrite(ledPIN3,HIGH);
    }

   
    if ((val > 500) && (old_val > 500)){
      state=1-state;
    }
  
  old_val = val; // valor del antiguo estado
    if (state==1){
      lcd.backlight(); // enciende el LCD
    }
    else{
      lcd.noBacklight(); // apagar el LCD
    }

  
        delay (1000); //10 minutos 6000000
}


 /////////////////////////////////////////////////////22-02-2022/////////////////////////////////////////////////////////////////////////////////////////////////////
 /////////////////////////////////////////////////////JORGE DE VIVAR ADRADA//////////////////////////////////////////////////////////////////////////////////////////
