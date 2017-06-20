import turtle
import os
from datetime import datetime


def interleave(xs, ys):
    return [val for pair in zip(xs, ys) for val in pair]

def gencolor(word, n=4):
    colors = [word]
    for i in range(1, 4+1):
        colors.append(word+str(i))
    return colors

CUSTOM = ['yellow', 'blue', 'red', 'green', 'orange', 'purple', 'black']
AQUA = gencolor('aquamarine')
AZURE = gencolor('azure')
BISQUE = gencolor('bisque')
BLUE = gencolor('blue')
SHELL = gencolor('seashell')
SALMON = gencolor('salmon')

def square(length):
    for i in range(4):
        turtle.forward(length)
        turtle.left(90)

def repeat(n, func):
    for i in range(n):
        func()

def squaggle():
    turtle.forward(50)
    turtle.right(150)
    turtle.forward(60)
    turtle.right(100)
    turtle.forward(30)
    turtle.right(90)
    

def polygon(length, angle, nsides):
    for i in range(nsides):
        turtle.forward(length)
        turtle.left(angle)

def multiple(start, end, interval, angle):
    for number in range(start, end, interval):
        polygon(number, angle, 360 / angle)
        
        
def draw(x=4):
    #~ colors = interleave(AQUA, SALMON)
    colors = CUSTOM
    for i, color in enumerate(colors):
        turtle.color(color)
        #~ multiple(50, 200, 1 + (5*i), 144)
        multiple(50, 200, 1 + (5*i), 110)

def cmd(shell, *args, **kwds):
    os.system(shell.format(*args, **kwds))

def pdf(screen):
    name = datetime.now().strftime('%Y-%m-%d-%H-%M')
    ps = "{}.ps".format(name)
    screen.getcanvas().postscript(file=ps)
    cmd('ps2pdf {name}.ps {name}.pdf', name=name)
    cmd('mv {}.pdf ~/shared', name)
    cmd('rm {}.ps', name)


def main():
    screen = turtle.Screen()
    screen.setworldcoordinates(-500, -500, 500, 500)
    turtle.home()
    turtle.pensize(2)
    turtle.speed(0)
    draw()
    #repeat(20, squaggle)
    pdf(screen)
    turtle.done()

if __name__ == '__main__': main()

    