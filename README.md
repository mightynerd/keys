The communication from the keyboard is done directly with the Intel 80C32-chip over 5V TTL serial at 9600 baud. Opon pressing a key, the keyboard transmits data in one of two possible formats:

- A single byte, representing the pressed key on (most of) the QWERTY section
- Three bytes, with the first being 1B (the escape byte) and the following two bytes representing a key outside of the QWERY-section.

The keyboard normally only transmits a single event for a key press (no separate key pressed and key released events). The Shift, CTRL and ALT keys are the only exceptions and work according to the following:
|Key|Second byte|Third byte|Meaning|
|---|-----------|----------|-------|
|CTRL|0x2C|0x64|CTRL pressed|
|CTRL|0x21|0x64|CTRL released|
|ALT|0x2C|0x65|ALT pressed|
|ALT|0x21|0x65|ALT released|
|Shift|0x2C|0x66|Shift pressed|
|Shift|0x21|0x66|Shift released|


# Code generation
This project now suddently involves a C code generator to simplify key definitions. This is done using a simple key definition language defined in `/bnf/keys.cf`.

Example of a key definition file:
```
# Map a single-byte key `37` to the keyboard key `KEY_RETURN`
single 37 => keyboard "KEY_RETURN" ;

# Map a three-byte key `2A 3C` (last two bytes) to press the keyboard
# key `KEY_LEFT_CTRL` followed by the consumer device key `MEDIA_PLAY_PAUSE`
double 2A 3C
  => keyboard "KEY_LEFT_CTRL",
     consumer "MEDIA_PLAY_PAUSE" ;

# Map the key `2A 30` to write "ABcd"
double 2A 30 => 
  keyboard down "KEY_LEFT_SHIFT" ,
  keyboard "KEY_A" ,
  keyboard "KEY_B" ,
  keyboard up "KEY_LEFT_SHIFT" ,
  keyboard "KEY_C" ,
  keyboard "KEY_D" ;
```

In order to run the generator:
1. Install ghc, bnfc, happy & alex.
2. Inside `/bnf`, run `bnfc -d -m keys.cf && make`.
3. The `Gen.hs` program can now parse `/bnf/keys.def` and generate `/bnf/out.c`.

The generator generates four struct arrays:
```c++
struct target {
  byte type;
  KeyboardKeycode code;
};

const PROGMEM struct target a[256][6] = {...};
const PROGMEM struct target b[256][2] = {...};
const PROGMEM struct target c[256][2] = {...};
const PROGMEM struct target r[256][2] = {...};
```
The `a`, `b` and `c` arrays contain the definitions for three-byte keys with the second byte `0x2A`, `0x2B` and `0x2C` respectively. The `r` array contains the definitions for single byte keys.

Each `target` instance represents a keyboard/mouse/consumer action. The first dimention of a target array serves as map between the third byte of a key and an array of actions. For example, the actions for the key with the second byte `0x2A` and the third byte `0x38` (first byte in the case of single byte keys) are defined in `a[0x38]`. The second dimention of a target array is an array of actions to be executed in sequence, always ending with a null target `{0,0}`, indicating its end.

Note that the arrays are stored in the Arduino's program memory and need to be dynamically copied to RAM during runtime. This is done since the arrays wouldn't fit in an Arduino Micro's 2,5kiB of RAM.

The `type` byte's 4 most significant bits represent the action while the 4 least significant bits represent the device according to the following:
- Actions:
  - Click: `0x1`
  - Up: `0x2`
  - Down: `0x3`
- Devices:
  - Keyboard: `0x1`
  - Mouse: `0x2`
  - Consumer: `0x3`

[NicoHood's HID library](https://github.com/NicoHood/HID) is used to make the Arduino act as a USB HID device, or more specifically, a keyboard, mouse and consumer device at the same time.

# Notes
The keyboard has a built in piezo speaker that makes a beep on every key press by default. I accidentally found out that this behavior can be toggled with CTRL+k. I discovered this by noticing that, when holding CTRL, the CTRL down event is sent over serial but now the "k" key. There is an additional key combination, CTRL+b, that's not sent anything over serial.
