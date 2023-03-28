#define HID_CUSTOM_LAYOUT
#define LAYOUT_SWEDISH
#include "HID-Project.h"

// ----
#define DEVICE_KEYBOARD 0x01
#define DEVICE_MOUSE 0x02
#define DEVICE_CONSUMER 0x03

struct target {
  byte type;
  byte code;
};

target a[256][2];
target b[256][1];
target c[256][1];
target r[256][1];

void init_keys() {
  a[0x3C][0].type = 0x01;
  a[0x3C][0].code = 0x01;
  a[0x3C][1].type = 0x01;
  a[0x3C][1].code = 0x01;
  // Q
  r[0x71][0].type = DEVICE_KEYBOARD;
  r[0x71][0].code = 'a';
}

void print_target(target* t) {
  Serial.print("  Type: ");
  Serial.print(t->type, HEX);
  Serial.print(", code: ");
  Serial.print(t->code, HEX);
  Serial.print("\n");
}

void execute_target(target* t) {
  switch (t->type) {
    case DEVICE_KEYBOARD:
      Serial.println("keyboard");
      Keyboard.write(t->code);
      break;
    case DEVICE_MOUSE:
      Mouse.click(t->code);
      break;
    case DEVICE_CONSUMER:
      Consumer.press(t->code);
      break;
  }
}

// ---

#define ESCAPE 0x1B

void setup() {
  Serial.begin(9600);
  Serial.println("Initializing keyboard serial at 9600 baud");
  Serial1.begin(9600, SERIAL_8N1);

  init_keys();

  Serial.println("Initializing keyboard library");
  Keyboard.begin();
  //Consumer.begin();
  //Mouse.begin();
  Serial.println("Ready");
}

void loop() {
  if (Serial1.available() > 0) {
    byte first = Serial1.read();

    if (first == ESCAPE) {
      return doubl();
    } else {
      return single(first);
    }
  }
}



void single(byte key) {
  for (byte i = 0; i < sizeof(r[key]) / 2; i++) {
    Serial.print("Single ");
    Serial.print(key, HEX);
    Serial.print("\n");
    print_target(&r[key][i]);
    execute_target(&r[key][i]);
  }
}


void doubl() {
  // Wait for two more bytes
  while (Serial1.available() < 2)
    ;

  byte second = Serial1.read();
  byte third = Serial1.read();

  target* t;
  switch (second) {
    case 0x2A:
      t = a[third];
    case 0x2B:
      t = b[third];
    case 0x2C:
      t = c[third];
  }

  for (byte i = 0; i < sizeof(*t) * 2; i++) {
    Serial.print(sizeof(*t));
  }
}