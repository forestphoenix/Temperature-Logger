EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:switches
LIBS:relays
LIBS:motors
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:ESP32-footprints-Shem-Lib
LIBS:Sensor
LIBS:ESP-Logger-HW-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L ESP32-WROOM ESP1
U 1 1 5F90835A
P 2900 3000
F 0 "ESP1" H 2200 4250 60  0000 C CNN
F 1 "ESP32-WROOM" H 3400 4250 60  0000 C CNN
F 2 "ESP32-footprints-Lib:ESP32-WROOM" H 3250 4350 60  0001 C CNN
F 3 "" H 2450 3450 60  0001 C CNN
	1    2900 3000
	1    0    0    -1  
$EndComp
$Comp
L VCC #PWR01
U 1 1 5F9085DA
P 800 950
F 0 "#PWR01" H 800 800 50  0001 C CNN
F 1 "VCC" H 800 1100 50  0000 C CNN
F 2 "" H 800 950 50  0001 C CNN
F 3 "" H 800 950 50  0001 C CNN
	1    800  950 
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR02
U 1 1 5F9086C7
P 900 4400
F 0 "#PWR02" H 900 4150 50  0001 C CNN
F 1 "GND" H 900 4250 50  0000 C CNN
F 2 "" H 900 4400 50  0001 C CNN
F 3 "" H 900 4400 50  0001 C CNN
	1    900  4400
	1    0    0    -1  
$EndComp
$Comp
L DHT11 DHT1
U 1 1 5F908EE1
P 4850 2150
F 0 "DHT1" H 4700 2400 50  0000 C CNN
F 1 "DHT11" H 5000 2400 50  0000 C CNN
F 2 "Sensor:Aosong_DHT11_5.5x12.0_P2.54mm" H 4850 1750 50  0001 C CNN
F 3 "" H 5000 2400 50  0001 C CNN
	1    4850 2150
	1    0    0    -1  
$EndComp
$Comp
L R DHT_Pullup1
U 1 1 5F909000
P 4300 1500
F 0 "DHT_Pullup1" V 4380 1500 50  0000 C CNN
F 1 "1K" V 4300 1500 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0204_L3.6mm_D1.6mm_P1.90mm_Vertical" V 4230 1500 50  0001 C CNN
F 3 "" H 4300 1500 50  0001 C CNN
	1    4300 1500
	0    1    1    0   
$EndComp
Wire Wire Line
	800  950  800  2400
Wire Wire Line
	800  2400 1950 2400
Wire Wire Line
	900  4400 900  3700
Wire Wire Line
	900  3700 1950 3700
Wire Wire Line
	4850 1850 4850 1200
Wire Wire Line
	4850 1200 800  1200
Connection ~ 800  1200
Wire Wire Line
	4150 1500 4050 1500
Wire Wire Line
	4050 1500 4050 1200
Connection ~ 4050 1200
Wire Wire Line
	4450 1500 5250 1500
Wire Wire Line
	5250 1500 5250 3350
Wire Wire Line
	5250 2150 5150 2150
Wire Wire Line
	4850 2450 4850 4300
Wire Wire Line
	4850 4300 900  4300
Connection ~ 900  4300
Wire Wire Line
	5250 3350 3800 3350
Connection ~ 5250 2150
$EndSCHEMATC