from blessed import Terminal

term = Terminal()
keymap = {'w':0, 'e':1, 'r':2, 'i':3, 'o':4, 'p':5}
alphabet = '0123456789abcdefghijklmnopqrstuvwxyz'

def kb(a, b):
  return alphabet[keymap[a] * 6 + keymap[b]]

def write(s):
  print(s, end=term.on_black)

def getch():
  with term.cbreak(), term.hidden_cursor():
    return term.inkey()

def handle_specials(c):
    if c == "'":
      write(term.move_left + ' ' + term.move_down)
    elif c == 'a':
      write(term.move_left + ',')
    elif c == ' ':
      write(term.move_left + ' ')
    else:
      write(term.move_left)

while True:
  write(term.on_bright_white + ' ')
  if (a := getch()) not in keymap:
    handle_specials(a)
    continue
  write(term.move_left + term.on_green + ' ' + term.on_black)
  if (b := getch()) not in keymap:
    handle_specials(b)
    continue
  char = kb(a, b)
  print(term.move_left + char, end='')

