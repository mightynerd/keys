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
