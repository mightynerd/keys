#include <Keyboard.h>
#include <Keyboard_sv_SE.h> // extra key definitions from Swedish layout

char MODIFIER = 0x1B;
char MOD[256];
char KEY[256];
char E[256];
char PRESS = 0x2C;
char RELEASE = 0x21;

// Regular key remap
void initKEY()
{
  KEY[0xD] = KEY_RETURN;
  KEY[0x7F] = KEY_BACKSPACE;
}

// Keys with three byte, only press events
void initExtra()
{
  E[0x68] = KEY_ESC;
}

// Modifier keys (with separate events for press/release)
void initMOD()
{
  // Modifiers
  MOD[0x66] = KEY_LEFT_SHIFT;
  MOD[0x64] = KEY_LEFT_CTRL;
  MOD[0x65] = KEY_LEFT_ALT;
}

void setup()
{
  Serial.begin(9600);
  Serial.println("Initializing keyboard serial at 9600 baud");
  Serial1.begin(9600, SERIAL_8N1);
  initKEY();
  initMOD();
  initExtra();
  Serial.println("Initializing keyboard library");
  Keyboard.begin(KeyboardLayout_sv_SE);
  Serial.println("Ready");
}

void loop()
{
  if (Serial1.available() > 0)
  {
    char d = Serial1.read();
    if (d == MODIFIER)
    {
      return modifier();
    }
    else
    {
      return qwerty(d);
    }
  }
}

void qwerty(char c)
{
  char key = KEY[c];
  Serial.print(key);Serial.print(" - ");
  if (key == 0)
  {
    Keyboard.print(key);
  }
  else
  {
    Keyboard.print(c);
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
void modifier()
{
  // Wait for two more bytes
  while (Serial1.available() < 2)
    ;

  char mod = Serial1.read();
  char key = Serial1.read();

  // This is a remapped key
  if (KEY[key] != 0) return qwerty(KEY[key]);

  char code = MOD[key];

  if (code == 0)
  {
    Serial.print("Unknown modifier ");
    Serial.print(key, HEX);
    Serial.print("\n");
    return;
  }
  else
  {
    Serial.print("Mod: ");
    Serial.print(mod, HEX);
    Serial.print(" ");
    Serial.print(key, HEX);
    Serial.print(" ");
    Serial.print(code, HEX);
    Serial.print("\n");
  }

  if (mod == PRESS)
  {
    Keyboard.press(code);
    Serial.print("Pressing ");
    Serial.print(code, HEX);
    Serial.print("\n");
  }
  else if (mod == RELEASE)
  {
    Keyboard.release(code);
    Serial.print("Releasing ");
    Serial.print(code, HEX);
    Serial.print("\n");
  }
}
