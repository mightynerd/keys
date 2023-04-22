#define HID_CUSTOM_LAYOUT
#define LAYOUT_SWEDISH
#include "HID-Project.h"

// Escape from single byte -> three byte key event
#define ESCAPE 0x1B

KeyboardKeycode modifiers[3] = {KEY_LEFT_CTRL, KEY_LEFT_ALT, KEY_LEFT_SHIFT};
#define MODIFIER_PRESSED 0x2C
#define MODIFIER_RELEASED 0x21

#define DEVICE_KEYBOARD 0x01
#define DEVICE_MOUSE 0x02
#define DEVICE_CONSUMER 0x03

struct target {
  KeyboardKeycode type;
  KeyboardKeycode code;
};

const PROGMEM struct target a[256][2] = {};
const PROGMEM struct target b[256][2] = {};
const PROGMEM struct target c[256][2] = {};
const PROGMEM struct target r[256][2] = {};

// Executes a single target assuming it's defined
void execute_target(const target* t) {
  Serial.print(" -> Type: ");
  Serial.print(t->type, HEX);
  Serial.print(", code: ");
  Serial.print(t->code, HEX);
  Serial.print("\n");

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
}

// Executes an array of targets until reaching the null target {0, 0}
// Returns the number of executed targets
byte execute_targets(const target *t) {
  byte i = 0;
  while (true) {
    target current;
    memcpy_P(&current, &t[i], sizeof(target));

    // This is a null target
    if (current.type == 0) return i;

    execute_target(&current);
    i++;
  }
}

void single(int key) {
  byte size = sizeof(r[key]) / sizeof(target);

  Serial.print("Single 0x");
  Serial.print(key, HEX);
  Serial.print(", size ");
  Serial.print(size);
  Serial.print("\n");

  const byte executed = execute_targets(r[key]);

  // Simple write the key if there is no definition
  if (executed < 1) Keyboard.write(key);
}


// Handles modifiers (Shift, CTRL, ALT)
// Return value indicates whether the event has been handled
bool modifier (byte second, byte third) {
  if (second != MODIFIER_PRESSED && second != MODIFIER_RELEASED || (third > 0x66 || third < 0x64)) return false;

  KeyboardKeycode key = modifiers[third - 0x64];
  if (second == MODIFIER_PRESSED) {
    Serial.println(F("Modifier pressed"));
    Keyboard.press(key);
  } else {
    Serial.println(F("Modifier pressed"));
    Keyboard.release(key);
  }

  return true;
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

  // Handle modifiers
  if (modifier(second, third)) return;

  execute_targets(second == 0x2A ? a[third] : second == 0x2B ? b[third] : c[third]);
}

void setup() {
  Serial.begin(9600);
  Serial.println(F("Initializing keyboard serial at 9600 baud"));
  Serial1.begin(9600, SERIAL_8N1);

  Serial.println(F("Initializing keyboard library"));
  Keyboard.begin();
  Consumer.begin();
  Mouse.begin();
  Serial.println(F("Ready"));
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

    Serial.println("stop\n");
  }
}
