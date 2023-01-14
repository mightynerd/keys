#include <Keyboard.h>
#include <Keyboard_sv_SE.h>  // extra key definitions from Swedish layout

char MODIFIER = 0x1B;
char MOD[256];
char KEY[256];
char E[256];
char PRESS = 0x2C;
char RELEASE = 0x21;

// Regular key remap
void initKEY() {
  for (int i = 0; i < 256; i++)
    KEY[i] = 0;

  KEY[0xD] = KEY_RETURN;
  KEY[0x7F] = KEY_BACKSPACE;
  KEY[0x7D] = KEY_A_RING;
  KEY[0x7B] = KEY_A_UMLAUT;
  KEY[0x7C] = KEY_O_UMLAUT;
  KEY[0x60] = KEY_ACUTE_ACC;
}

// Keys with three byte, only press events
void initExtra() {
  for (int i = 0; i < 256; i++)
    E[i] = 0;

  E[0x68] = KEY_ESC;

  // F-keys (F1 (0x73) => KEY_F1 (0xC2), offset 0x4F)
  for (char fbase = 0x73; fbase <= 0x7E; fbase++)
    E[fbase] = fbase + 0x4F;
}

// Modifier keys (with separate events for press/release)
void initMOD() {
  // Modifiers
  for (int i = 0; i < 256; i++)
    MOD[i] = 0;

  MOD[0x66] = KEY_LEFT_SHIFT;
  MOD[0x64] = KEY_LEFT_CTRL;
  MOD[0x65] = KEY_LEFT_ALT;
}

bool READ_ONLY = false;
void setup() {
  Serial.begin(9600);
  Serial.println("Initializing keyboard serial at 9600 baud");
  Serial1.begin(9600, SERIAL_8N1);
  initKEY();
  initMOD();
  initExtra();
  Serial.println("Initializing keyboard library");
  Keyboard.begin(KeyboardLayout_sv_SE);
  Keyboard.println("tja man");
  Serial.println("Ready");
}

void loop() {
  if (Serial1.available() > 0) {
    char d = Serial1.read();
    if (READ_ONLY) {
      Serial.print(d, HEX);
      return;
    }
    if (d == MODIFIER) {
      return modifier();
    } else {
      return qwerty(d);
    }
  }
}

void qwerty(char c) {
  char key = KEY[c];
  Serial.print(key, HEX);
  Serial.print(" - ");
  if (key == 0) {
    Keyboard.print(c);
  } else {
    Keyboard.print(key);
  }

  // Log
  Serial.print("K: ");
  Serial.print(c);
  Serial.print("|");
  Serial.print(c, HEX);
  Serial.print("\n");
}

// The keyboard sends two bytes:
// - A modifier byte
// - A key code
void modifier() {
  // Wait for two more bytes
  while (Serial1.available() < 2)
    ;

  char mod = Serial1.read();
  char key = Serial1.read();

  // This is a remapped key
  if (KEY[key] != 0) return qwerty(KEY[key]);

  // Press/release event
  if (press_release(mod, key)) return;

  char e = E[key];

  if (e == 0) {
    Serial.print("Unknown modifier ");
    Serial.print(mod, HEX);
    Serial.print(", ");
    Serial.print(key, HEX);
    Serial.print("\n");
    return;
  }

  Keyboard.print(e);
}

// Returns a bool indicating whether they event has been handled
bool press_release(char mod, char key) {
  char code = MOD[key];
  if (code == 0) return false;

  if (mod == PRESS) {
    Keyboard.press(code);
    Serial.print("Pressing ");
    Serial.print(code, HEX);
    Serial.print("\n");
  } else if (mod == RELEASE) {
    Keyboard.release(code);
    Serial.print("Releasing ");
    Serial.print(code, HEX);
    Serial.print("\n");
  }

  return true;
}
