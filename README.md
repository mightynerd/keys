The communication from the keyboard is done directly with the Intel 80C32-chip over 5V TTL serial at 9600 baud. Opon pressing a key, the keyboard transmits data in one of two possible formats:

- A single byte, represending the pressed key on the QWERTY section
- Three bytes, with the first being 1B, the second being 2C (key pressed) or 21 (key released, applies only to SHIFT, CTRL and Shift) and the third represending the key pressed.

1B: Modifier

QWERTY:
- Backspace: 2F
- Enter: D
- Lower enter: 1B 2C 67

Modifiers:
- ESC: 1B 2C 68
- CTRL press:   1B 2C 64
- CTRL release: 1B 21 64

- Shift press:    1B 2C 66
- Shift release:  1B 21 66

- Alt press:  1B 2C 65
- Alt release: 1B 21 65

# Code generation
This project now suddently involves a C code generator to simplify key definitions. This is done using a simple key definition language defined in `/bnf/keys.cf`.

Simple example of a key definition file:
```
# Remap a simple key `37` to the keyboard key `KEY_RETURN`
single 37 => keyboard KEY_RETURN ;

# Remap a double (two byte) key `2A 3C` to press the keyboard
# key `KEY_LEFT_CTRL` followed by the consumer device key `MEDIA_PLAY_PAUSE`
double 2A 3C
  => keyboard KEY_LEFT_CTRL,
     consumer MEDIA_PLAY_PAUSE ;
```

In order to run the generator:
1. Install ghc, bnfc, happy & alex.
2. Inside `/bnf`, run `bnfc -d -m keys.cf && make`
3. The `Gen.hs` program can now parse `/bnf/keys.def` and generate `/bnf/out.c`
