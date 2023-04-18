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

const PROGMEM struct target a[256][3] = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{{DEVICE_KEYBOARD, 'k'},{DEVICE_KEYBOARD, 'u'},{DEVICE_KEYBOARD, 'k'}},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}};
const PROGMEM struct target b[256][1] = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}};
const PROGMEM struct target c[256][1] = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}};
const PROGMEM struct target r[256][3] = {{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{{DEVICE_KEYBOARD, 0xA3},{DEVICE_CONSUMER, 0xA4}},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{{DEVICE_KEYBOARD, 'a'},{DEVICE_KEYBOARD, 'b'},{DEVICE_KEYBOARD, 'c'}},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}};

void print_target(const target* t) {
  //if (t->type == 0) return;

  Serial.print(" -> Type: ");
  Serial.print(t->type, HEX);
  Serial.print(", code: ");
  Serial.print(t->code, HEX);
  Serial.print("\n");
}

bool execute_target(const target* t) {
  // Return if the target is not defined
  print_target(t);
  if (t->type == 0) {
    Serial.println("Target undefined");
    return false;
  } else {
    //print_target(t);
  }

  switch (t->type) {
    case DEVICE_KEYBOARD:
      Keyboard.write(t->code);
      break;
    case DEVICE_MOUSE:
      Mouse.click(t->code);
      break;
    case DEVICE_CONSUMER:
      Consumer.press(t->code);
      break;
  }

  return true;
}
// ---

#define ESCAPE 0x1B

void setup() {
  Serial.begin(9600);
  Serial.println("Initializing keyboard serial at 9600 baud");
  Serial1.begin(9600, SERIAL_8N1);

  Serial.println("Initializing keyboard library");
  Keyboard.begin();
  Consumer.begin();
  Mouse.begin();
  Serial.println("Ready");
}

void loop() {
  if (Serial1.available() > 0) {
    // Read first byte
    int first = Serial1.read();

    if (first == ESCAPE) {
      // More bytes follow
      doubl();
    } else {
      single(first);
    }

    Serial.print('\n');
  }
}

void single(int key) {
  byte size = sizeof(r[key]) / sizeof(target);

  Serial.print("Single 0x");
  Serial.print(key, HEX);
  Serial.print(", size ");
  Serial.print(size);
  Serial.print("\n");

  for (int i = 0; i < sizeof(r[key]) / sizeof(target); i++) {
    // Copy the current target to RAM
    target current;
    memcpy_P(&current, &r[key][i], sizeof(target));

    bool executed = execute_target(&current);

    // Simple write the key if there is no definition
    if (!executed && i == 0) {
      Keyboard.write(key);
      return;
    }
  }
}

byte execute_targets(const target *t, byte size) {
  byte executed = 0;

  for (byte i = 0; i < size; i++) {
    target current;
    memcpy_P(&current, &t[i], sizeof(target));

    bool current_executed = execute_target(&current);
    if (current_executed) executed++;
    else return executed;
  }
}

void doubl() {
  // Wait for two more bytes
  while (Serial1.available() < 2)
    ;

  byte second = Serial1.read();
  byte third = Serial1.read();

  Serial.print("Double: ");
  Serial.print(second, HEX);
  Serial.print(", ");
  Serial.print(third, HEX);
  Serial.print('\n');

  target *t;
  byte length = 0;

  switch (second) {
    case 0x2A:
      t = a[third];
      length = sizeof(a[third]) / sizeof(target);
    case 0x2B:
      t = b[third];
      length = sizeof(b[third]) / sizeof(target);
    case 0x2C:
      t = c[third];
      length = sizeof(c[third]) / sizeof(target);
  }

  for (byte i = 0; i < length; i++) {
    // Copy the current target to RAM
    target current;
    memcpy_P(&current, t[i], sizeof(target));
    
    execute_target(&current);
  }
}
