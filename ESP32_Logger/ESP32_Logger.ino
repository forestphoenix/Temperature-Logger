/**
 * BasicHTTPClient.ino
 *
 *  Created on: 24.05.2015
 *
 */

#ifndef ARDUINO
#define RTC_DATA_ATTR
#endif

#include <Arduino.h>
#include <time.h>
#include <DHTesp.h>

#include <WiFi.h>
#include <WiFiMulti.h>

#include <HTTPClient.h>
#include <ESPmDNS.h>

#include "TempLoggerSM.h"
#include "TransmitDataSM.h"

// HW-Config

constexpr int dhtPin = 4;
constexpr int statusLed   = 17;
constexpr int transmitLed = 18;
constexpr int errorLed    = 19;
DHTesp dht;

const char* ssid = "Pressurized_Network";
const char* password = "1Gerti.Douglas.Richardson";

WiFiMulti wifiMulti;

// Persistent Memory

unsigned long long msToSleepTime(unsigned long long sleepTime)
{
    return sleepTime * 1000;
}

RTC_DATA_ATTR DHTesp::DHT_MODEL_t dhtModel = DHTesp::AUTO_DETECT;
RTC_DATA_ATTR TempLoggerSM mainSM{TempLoggerConfig()};
RTC_DATA_ATTR TransmitDataSM transmitSM;

void setup()
{
    Serial.begin(115200);
    delay(1000);
    
    pinMode(statusLed, OUTPUT);
    pinMode(transmitLed, OUTPUT);
    pinMode(errorLed, OUTPUT);
    
    digitalWrite(statusLed, HIGH); // Will be off during deep sleep

    dht.setup(dhtPin, dhtModel);
}

bool browseService(const char * service, const char * proto, IPAddress *ip, uint16_t *port){
    Serial.printf("Browsing for service _%s._%s.local. ... ", service, proto);
    int n = MDNS.queryService(service, proto);

    bool found;
    if (n == 0) {
        Serial.println("no services found");
        found = false;
    } else {
        Serial.print(n);
        Serial.println(" service(s) found, using first discovered");
        for (int i = 0; i < n; ++i) {
            // Print details for each service found
            Serial.print("  ");
            Serial.print(i + 1);
            Serial.print(": ");
            Serial.print(MDNS.hostname(i));
            Serial.print(" (");
            Serial.print(MDNS.IP(i));
            Serial.print(":");
            Serial.print(MDNS.port(i));
            Serial.println(")");
        }

        found = true;
        *ip = MDNS.IP(0);
        *port = MDNS.port(0);
    }
    Serial.println();
    return found;
}

TransmitDataEvent runTransmitData(TransmitDataAction const& action)
{
    switch(action.action)
    {
        case TransmitDataAction::Action::ConnectWifi:
            {
                Serial.println("TransmitDataAction::Action::ConnectWifi: connecting...");
                wifiMulti.addAP(ssid, password);
                
                bool connected = false;
                unsigned attempt = 1;
                while(attempt < MAX_WIFI_CONNECT_ATTEMPTS && not connected)
                {
                    if(wifiMulti.run() == WL_CONNECTED)
                    {
                        Serial.println("connected");
                        connected = true;
                    }
                    else
                    {
                        Serial.println("connection failed, retrying");
                        delay(1000);
                        
                        attempt++;
                    }
                }
                
                if(connected)
                {
                    Serial.print("Connected to wifi, current IP: ");
                    Serial.println(WiFi.localIP().toString());
                    return TransmitDataEvent::dataless(TransmitDataEvent::Event::WifiConnected);
                }
                else
                {
                    Serial.println("Failed to connect wifi");
                    return TransmitDataEvent::errorOccurred(ErrorCode::WifiConnectFailed);
                }
            }
            break;
            
        case TransmitDataAction::Action::LookupService:
            {
                Serial.println("TransmitDataAction::Action::LookupService: start lookup...");
                
                if (!MDNS.begin("ESP32_Browser"))
                {
                    Serial.println("Failed to set up Mdns");
                    return TransmitDataEvent::errorOccurred(ErrorCode::ServiceLookupFailed);
                }
                
                IPAddress serviceIp;
                uint16_t servicePort;
                if(not browseService("templog-server", "tcp", &serviceIp, &servicePort))
                {
                    Serial.println("Failed to locate service");
                    return TransmitDataEvent::errorOccurred(ErrorCode::ServiceLookupFailed);
                }
                
                return TransmitDataEvent::serviceFound(serviceIp, servicePort);
            }
            break;
            
        case TransmitDataAction::Action::TransmitData:
            {
                Serial.print("TransmitDataAction::Action::TransmitData: transmitting ");
                Serial.print(action.readingsCount);
                Serial.println(" readings");
                
                String uniqueId;
                for (size_t i = 0; i < UniqueIDsize; i++)
                {
                    uniqueId += String(UniqueID[i], HEX);
                }
                
                String payload = String("[");
                for(int idxReading = 0; idxReading < action.readingsCount; idxReading++)
                {
                    TemperatureReadout reading = action.readings[idxReading];
                    
                    if(idxReading > 0)
                    {
                        payload += ", ";
                    }
                    
                    payload +=  String("{\"temp\": ")        + String(reading.temperature) +
                                String(", \"humidity\": ")   + String(reading.humidity) + 
                                String(", \"deviceTime\": ") + String(reading.timestamp) + 
                                String(", \"device\": \"")   + uniqueId + String("\"") +
                                String("}");
                }
                payload += String("]");
                
                // Actual transmission
                
                
                HTTPClient http;
    
                Serial.print("[HTTP] begin... on address ");
                IPAddress serviceIp{action.ip4Address};
                Serial.print(serviceIp.toString());
                Serial.print(" and port: ");
                Serial.println(action.port);
                // configure traged server and url
                //http.begin("192.168.0.11/a/check", ca); //HTTPS
                http.begin(serviceIp.toString(), action.port, "/templog/v2/send"); //HTTP
        
                Serial.print("[HTTP] POST...\n");
                // start connection and send HTTP header
        
                Serial.print("Sending JSON: ");
                Serial.println(payload);
                
                int httpCode = http.POST(payload);
        
                // httpCode will be negative on error
                if(httpCode > 0) {
                    // HTTP header has been send and Server response header has been handled
                    Serial.printf("[HTTP] POST... code: %d\n", httpCode);
                    String payload = http.getString();
                    Serial.println(payload);
                    
                    // file found at server
                    if(httpCode == HTTP_CODE_OK) {
                        return TransmitDataEvent::dataless(TransmitDataEvent::Event::DataTransmitted);
                    }
                    else
                    {
                        return TransmitDataEvent::errorOccurred(ErrorCode::TransmitDataFailed);
                    }
                } else {
                    Serial.printf("[HTTP] POST... failed, error: %s\n", http.errorToString(httpCode).c_str());

                    return TransmitDataEvent::errorOccurred(ErrorCode::TransmitDataFailed);
                }
            }
            break;
    }
}

void blinkOutCode(uint32_t code)
{
    Serial.print("ERROR: ");
    Serial.println(code);
    
    
    for(uint32_t blink = 0; blink <= code; blink++)
    {
        digitalWrite(errorLed, HIGH);
        delay(100);
        
        digitalWrite(errorLed, LOW);
        delay(100);
    }
    
    delay(1000);
}

TempLoggerEvent runTempLogger(TempLoggerAction const& action)
{
    switch (action.action)
    {
    case TempLoggerAction::Action::Initialize:
        transmitSM.run(TransmitDataEvent::dataless(TransmitDataEvent::Event::FirstStart), &runTransmitData);
                                                                                    
        if (dhtModel == DHTesp::AUTO_DETECT) // Not initialized before deep sleep
        {
            Serial.println("TempLoggerAction::Action::Initialize: Detecting sensor...");
            
            delay(1000); // According to the docs of the DHT-Lib, we may have to wait 1000 ms after auto-detection
            
            dht.getTemperature(); // Force at least one sensor read
            if (dht.getStatus() == DHTesp::ERROR_NONE)
            {
                Serial.println("TempLoggerAction::Action::Initialize: OK");
                
                dhtModel = dht.getModel();

                // Autodetection will require an extra read.
                // Since I want to save as much energy as possible, we cache the device type 
                // in RTC memory for the next wake-up
                return TempLoggerEvent::dataless(TempLoggerEvent::Event::Initialized);
            }
            else
            {
                Serial.print("TempLoggerAction::Action::Initialize: ERROR: ");
                Serial.println(dht.getStatus());
                
                return TempLoggerEvent::errorOccurred(ErrorCode::DhtError);
            }
        }
        else
        {
            Serial.println("TempLoggerAction::Action::Initialize: SKIP");
            
            // Since the sensor already worked, we assume everything will continue working.
            return TempLoggerEvent::dataless(TempLoggerEvent::Event::Initialized);
        }
        break;
        
    case TempLoggerAction::Action::TakeReading:
        {
            Serial.println("TempLoggerAction::Action::TakeReading: reading...");
            
            auto tempAndHumidity = dht.getTempAndHumidity();
            timeval currentTime;
            gettimeofday(&currentTime, nullptr);
            
            if(dht.getStatus() == DHTesp::ERROR_NONE)
            {
                Serial.println("TempLoggerAction::Action::TakeReading: OK");
                
                return TempLoggerEvent::readingTaken(
                    TemperatureReadout{
                        tempAndHumidity.temperature, 
                        tempAndHumidity.humidity,
                        currentTime.tv_sec
                    });
            }
            else
            {
                Serial.println("TempLoggerAction::Action::TakeReading: ERROR");
                
                return TempLoggerEvent::errorOccurred(ErrorCode::DhtError);
            }
        }
        break;
    
        case TempLoggerAction::Action::DeepSleep:
        {
            Serial.print("TempLoggerAction::Action::DeepSleep: Starting deep sleep for :");
            Serial.println(DEEP_SLEEP_TIME_MS);
            
            esp_sleep_enable_timer_wakeup(msToSleepTime(DEEP_SLEEP_TIME_MS));
            esp_deep_sleep_start();
        }
        
        case TempLoggerAction::Action::TransmitData:
            {
                Serial.println("TempLoggerAction::Action::TransmitData: Starting transmission...");
                Serial.print("Starting transmit for ");
                Serial.print(action.readingsCount);
                Serial.println(" readings");
                
                digitalWrite(transmitLed, HIGH);
                TransmitDataResult result = transmitSM.run(TransmitDataEvent::startTransmit(action.readings, action.readingsCount), &runTransmitData);
                digitalWrite(transmitLed, LOW);
                
                switch(result.result)
                {
                case TransmitDataResult::Result::TransmitOk:
                    Serial.println("TempLoggerAction::Action::TransmitData: Transmit OK");
                    return TempLoggerEvent::dataless(TempLoggerEvent::Event::ReadingsTransmitted);
                case TransmitDataResult::Result::ErrorOccurred:
                    Serial.println("TempLoggerAction::Action::TransmitData: Transmit ERROR");
                    return TempLoggerEvent::errorOccurred(result.error);
                    
                default:
                    Serial.println("TempLoggerAction::Action::TransmitData: Unknown answer");
                    return TempLoggerEvent::errorOccurred(ErrorCode::StateMismatch);
                }
            }
            break;
            
        case TempLoggerAction::Action::BlinkOutSingleError:
            blinkOutCode(action.error);
            Serial.println("TempLoggerAction::Action::BlinkOutSingleError: error blinked out, continuing...");
            return TempLoggerEvent::dataless(TempLoggerEvent::Event::ErrorBlinkedOut);
            break;
            
        case TempLoggerAction::Action::BlinkOutError:
            for(;;)
            {
                blinkOutCode(action.error);
            }
    }
    
    return TempLoggerEvent::errorOccurred(ErrorCode::StateMismatch);
}

void loop()
{
    TempLoggerEvent initialEvent;
    initialEvent.event = TempLoggerEvent::Event::FirstStart;

    esp_sleep_wakeup_cause_t wakeup_reason = esp_sleep_get_wakeup_cause();
    if (wakeup_reason == ESP_SLEEP_WAKEUP_TIMER)
    {
        Serial.println("Woken from deep sleep *yawn*");
        
        initialEvent.event = TempLoggerEvent::Event::WakeByTimer;
    }
    
    mainSM.run(initialEvent, &runTempLogger);
}
