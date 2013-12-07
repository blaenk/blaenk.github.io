---
title: Binary and Hexadecimal
published: November 1, 2013
excerpt: A basic guide to binary and hexadecimal
toc: off
---

This is a very old guide I wrote when I was 15, in 2005. It gained some popularity at the time, with visitors coming in from Wikipedia and other sources. I only have it here because someone, after all these years, requested it.

### Binary

Binary is a base 2 numbering system that computers use. When we do something in the computer, for example, if we press the letter N, it will receive the binary equivalent, in ASCII most likely (_This is not entirely true, but for the sake of simplicity..._). I will try to teach you binary as well as I possibly can.

First of all, you know base 10, the numbering system we use, it goes from 0-9. With the numbers 0-9 we can write any number, right? Also remember this?

```
Hundreds | Tens | Ones
   9        5      1     = 951
```

We will use this in our learning process, so I hope you understood that.

So we know that base 10 has numbers 0-9, which uses 10 numbers (count them, 0 to 9), hence, base 10. Guess what, Binary uses 2 numbers! 0 and 1. 0 means false, and 1 means true. So, for example, (for you fast learners, you might learn binary the second you see this), instead of making columns like in the example I used earlier with base 10, we will use numbers, powers of 2. A bit in binary is one digit, for example, 1010 = 4 bits. An octet is 8 bits, or a byte.

Lets write the number 24 in binary:

```
128 | 64 | 32 | 16 | 8 | 4 | 2 | 1
 0    0     0   1    1   0   0   0 = 0001 1000
```

(**NOTE**: When we write in binary, please write in 4 bits separation, which gives a more organized look.)

Let me explain, I put 1s under the numbers we used, and 0s for the ones we didn't (remember, 1 = true, 0 = false). So to make the number 24, we don't need a 128, so put a 0, we don't either need a 64, put another 0, we don't need a 32...but ah! We do need 16! So we put a 1 under it, and we also put a 1 under 8, 16 + 8 = 24. So we put 0s under the rest.

I'll show you a tip that will make your decimal to binary conversions much easier. Lets say you want to write the number 224 in binary. You would subtract 224 by the biggest power of two that fits in there, in this case, 128 (256 is the next, and that doesn't fit in 224). And you would then subtract by the power of two before the power of two you just used. Keep this going until you were left with 0, watch:

```
  224
 -128 <----We use a 128, so circle that on your paper
 ----
  96
 -64  <----We use a 64, so circle that on your paper
 ----
  32
 -32  <----We use a 32, so circle that on your paper
 ----
  0
```

Okay, so we used a 128, 64, and 32. Now, it's so easy to put this into binary, watch:

```
128 | 64 | 32 | 16 | 8 | 4 | 2 | 1
 1     1    1    0   0   0   0   0 = 1110 0000
```

That seemed easy, but what if the next power of 2 doesn't fit in the number? Watch, lets write 165 in binary:

```
  165
 -128 <----We use a 128, so circle that on your paper.
 ---- (NOTE: 64 is the power of 2 before 128, but it doesn't)
  37  (fit in 37, so we use the next, 32)
 -32  <----We use a 32, so circle that on your paper.
 ----
  5
 -4   <----We use a 4, so circle that on your paper.
 ----
  1
 -1   <----We use a 1, so circle that on your paper.
 ----
  0
```

```
128 | 64 | 32 | 16 | 8 | 4 | 2 | 1
  1    0    1    0   0   1   0   1 = 1010 0101
```

When I mean the next power of two, I mean as in a power of two going down, I'm sorry for sounding confusing, for example, the next power of two after 128 would be 64, in our example, at least. Since we are subtracting the 'next power of two' from the result.

### Hexadecimal

I hope you read the binary section, I expect you did, if you don't understand something, read the binary section.

Hexadecimal is a base 16 numbering system, it uses numbers (and letters) 0-F, 0 being 0 and F being 15. Read the table and memorize it:

```
0 = 0
1 = 1
2 = 2
3 = 3
4 = 4
5 = 5
6 = 6
7 = 7
8 = 8
9 = 9
A = 10
B = 11
C = 12
D = 13
E = 14
F = 15
```

First of all, you must understand a common convention in hexadecimal notation. You'll typically see hexadecimal numbers preceded by **0x**, it just means that the number is hexadecimal, but the actual **0x** is irrelevant to the number. So **0x42FE** means that **42FE** is a hexadecimal number, we know that because of the **0x**, but again, ignore the **0x** when you're actually working with the number.

To convert from binary to hexadecimal is very easy, you first separate the binary number into nibble (4 bits). For example, the number 24, which is 00011000 in binary, you would set it up as 0001 1000. Then, what you do, is you convert each nibble into decimal, for example, for the first nibble, 0001, since it equals one in decimal, you would then convert the decimal into hexadecimal, and since in hex 1 is 1, you would write 1. Now, for the second nibble, 1000, in decimal is 8, and in hex its 8, so 0001 1000 in hexadecimal is 0x18.

Lets do another one, the number 224 which is 11100000 in binary, would be 1110 0000. The first nibble, 1110, in decimal is 14. So you would see what 14 is in hexadecimal, 14 in hex is E. Now the second nibble is 0000, so in decimal its 0, and in hex its 0. The final number in hex would be 0xE0.

To convert from hexadecimal to binary, all you do is get the number, for example, 0xE0, that we did in the last example. We then read from left to right, or right to left, it doesn't matter, as long as we maintain the order. So let's do it from right to left, since I know most of you would say that, okay, the first digit is 0, and in binary 0 is 0000. Note that each digit in hexadecimal is 4 bits, so even though we can express 0 in 000000, and also 0, we have to fill up 4 bits, so it would be 0000. The next digit, which is E, in decimal is 14, so in binary it would be:

```
 14
- 8 <----Using an eight.
---
  6
- 4 <----Using a four
---
  2
- 2 <----Using a 2
---
  0
```

Since we are using nibbles, we only need four bits, so:

```
8 | 4 | 2 | 1
1   1   1   0
```

I know that finding out 14 in binary was very easy, but I just did the whole thing so that others could understand the process.

So 0xE0 in binary is 1110 0000. Note that we converted the hex number from right to left, so we write it the same way, right to left. We found out the 0000 first, so we put it on the right, and we work our way to the left, in this case, 1110, which is 14. So again, the answer is 1110 0000.

To convert from hexadecimal to decimal, don't be tempted to just do the following:

Considering 0xE0, you would say, okay, from right to left. 0 in decimal is 0. And E in decimal in 14, so you'd think the answer is 140. WRONG! Please remember the right way to do it. First, you convert it to binary, and from the last example, we found out its 1110 0000. okay, now you set up the columns, unless you're a master at binary:

```
128 | 64 | 32 | 16 | 8 | 4 | 2 | 1
  1    1    1    0   0   0   0   0
```

So it's 128 plus 64 plus 32, in other words, it's the hexadecimal number in binary, to decimal. Lol. Which is 224 !

### Tips

#### Faster Hexadecimal to Decimal Conversion

You know that in decimal each place is a power of 10, including the first which is 10 to the power of 0. The same thing with hexadecimal: Each position/place is a power of 16. So **2CD** would be **(2 * 16^2^) + (C * 16^1^) + (D * 16^0^)** . First we have **(2 * 16^2^)** This is the third place, the 256s place. So we're going to do 2 _times_ 256, I showed it as 16^2^ for illustration purposes. Next we have the second place, the 16s place. So we are going to do **(C * 16^1^)**. To do this, we must realize that C is 12 in hexadecimal. So that means we are going to do 12 _times_ 16. Next we have the first place, the 1s place. We know that D is 13 in hexadecimal, so **(D * 16^0^)** is really just 13 times 16 to the power of 0, which we know is just one. So in reality it is just 13 times 1. This all comes down to be **512 + 192 + 13**, which equals **717**.

If you don't understand that, think about it this way. Remember for example that in the number 1337 (_Decimal_), you have 1 in the thousands place, that means you're using 1 thousand, 3 in the hundreds place, that means you're using 3 hundreds, 3 in the tens place, meaning you'll be using 3 10s, and 7 in the one's place meaning you'll be using 7 ones. This is what helped you understand binary. Now, just look at it this way. Every place in this number is a power of 10, since we are using the decimal number system. So we have the thousands place which is 10^3^, the hundreds place which is 10^2^, the tens place which is 10^1^, and the one's place which is 10^0^. Now, we could rewrite 1337 as **(1 * 10^3^) + (3 * 10^2^) + (3 * 10^1^) + (7 * 10^0^)**, right? This will come out to be **1000 + 300 + 30 + 7**, which equals **1337**. This is the exact same process as the one I'm using above.

If you still don't get it, give it another read, and if you still don't get it (_Don't rush through it, picture the example I'm giving_), come back later and re-read it, you're bound to understand it sooner or later and how simple the concept works.

#### Binary Notation

Binary can be expressed by appending **(2)** after the number. Example: 01011011(2)

Sometimes, the number is preceded by a **%**. Example: %0001

Finally, some languages express binary by appending a **b** to the number. Example: 10101101b

#### Hexadecimal Notation

Usually a hexadecimal number is preceded by **0x** to show that the following number is in hexadecimal. Example: 0x2CD

In HTML, (_As well as in CSS and other web languages_) color codes are in hexadecimal and are preceded by **#**, in the format **#RRGGBB** meaning that the first 2 digits from left to right control the red color, the second two digits control the green, and the last two, the blue. Two digits means 256 possibilities in hexadecimal. Example: #FF00FF (_Purple_)

Another way to express hexadecimal is by appending an **h**. Example: 2CDh

URLs also escape characters using character codes in hexadecimal, preceded by a **%**. For example, if you have a URL that contains a space, you won't see it, instead you will see the ASCII/Unicode representation of a space, the hexadecimal number 20. So you will see something like **http://somesite.com/This%20URL%20Contains%20Spaces**. Other characters are escaped as well. For more information, read "[URL Encoding](http://www.blooberry.com/indexdot/html/topics/urlencoding.htm)".

### ASCII

This section teaches you how to read ASCII in binary. ASCII stands for American Standard Code for Information Interchange. Each character in ASCII has a binary value, which is usually written in hex, for easier human readability. 65 is the beginning decimal value for a capital letter 'A'. Then 66 is B, and so on. Notice that this is the decimal value, you have to convert it to binary or hexadecimal according to your situation. A table of the ASCII values can be found at <http://www.klcconsulting.net/ascii.htm>. For example, blank in all caps in hex (which is what it's usually taken as) would be 0x42 0x4C 0x41 0x4E 0x4B. The computer would look at it in binary, though. So all that converted to binary would be what the computer would see when you type 'BLANK'. Interesting to know.

I hope you learned something new today, or sharpened your skills in this area. I hope I have taught all of you new people to this area, and hope it helps. If there is something wrong with my information, please email me. Also, if you found this helpful, please email me and saying thanks or something, so that I know to keep writing tutorials.

### What to do with Binary/Hexadecimal

Okay, so you learned binary and hexadecimal now, what can you do? Well, nowadays you can't do much, but wait, don't lose your hope. These number systems are still crucial to many things in computers, and give you an edge when you know them. They help you better understand things like subnets. These number systems also help in encryption, low-level programming, and other hardcore practices like hex editing.

Hope you had a great time learning. If anything seemed confusing, hard, or if any information is wrong, please email me or comment on this post.
