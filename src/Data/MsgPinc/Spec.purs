{-|
Module      : Data.MessagePack.Spec
Description : Message Pack specification values
Copyright   : (c) Rodrigo Setti, 2014
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : portable

Define, in a single place, all the message-pack specification binary type
markers.
-}
module Data.MsgPinc.Spec
  ( array16
  , array32
  , bin16
  , bin32
  , bin8
  , ext16
  , ext32
  , ext8
  , fals
  , fixarray
  , fixarrayMask
  , fixext1
  , fixext16
  , fixext2
  , fixext4
  , fixext8
  , fixmap
  , fixmapMask
  , fixstr
  , fixstrMask
  , float32
  , float64
  , int16
  , int32
  , int64
  , int8
  , map16
  , map32
  , negFixint
  , negFixintMask
  , nil
  , posFixint
  , posFixintMask
  , str16
  , str32
  , str8
  , tru
  , uint16
  , uint32
  , uint64
  , uint8
  ) where

posFixintMask ∷ Int
posFixintMask = 0x80 -- 10000000

negFixintMask ∷ Int
negFixintMask = 0xe0 -- 11100000

fixmapMask ∷ Int
fixmapMask = 0xf0 -- 11110000

fixarrayMask ∷ Int
fixarrayMask = 0xf0 -- 11110000

fixstrMask ∷ Int
fixstrMask = 0xe0 -- 11100000

posFixint ∷ Int
posFixint = 0x00 -- 0xxxxxxx

negFixint ∷ Int
negFixint = 0xe0 -- 111xxxxx

fixmap ∷ Int
fixmap = 0x80 -- 1000xxxx

fixarray ∷ Int
fixarray = 0x90 -- 1001xxxx

fixstr ∷ Int
fixstr = 0xa0 -- 101xxxxx

nil ∷ Int
nil = 0xc0 -- 11000000

fals ∷ Int
fals = 0xc2 -- 11000010

tru ∷ Int
tru = 0xc3 -- 11000011

bin8 ∷ Int
bin8 = 0xc4 -- 11000100

bin16 ∷ Int
bin16 = 0xc5 -- 11000101

bin32 ∷ Int
bin32 = 0xc6 -- 11000110

ext8 ∷ Int
ext8 = 0xc7 -- 11000111

ext16 ∷ Int
ext16 = 0xc8 -- 11001000

ext32 ∷ Int
ext32 = 0xc9 -- 11001001

float32 ∷ Int
float32 = 0xca -- 11001010

float64 ∷ Int
float64 = 0xcb -- 11001011

uint8 ∷ Int
uint8 = 0xcc -- 11001100

uint16 ∷ Int
uint16 = 0xcd -- 11001101

uint32 ∷ Int
uint32 = 0xce -- 11001110

uint64 ∷ Int
uint64 = 0xcf -- 11001111

int8 ∷ Int
int8 = 0xd0 -- 11010000

int16 ∷ Int
int16 = 0xd1 -- 11010001

int32 ∷ Int
int32 = 0xd2 -- 11010010

int64 ∷ Int
int64 = 0xd3 -- 11010011

fixext1 ∷ Int
fixext1 = 0xd4 -- 11010100

fixext2 ∷ Int
fixext2 = 0xd5 -- 11010101

fixext4 ∷ Int
fixext4 = 0xd6 -- 11010110

fixext8 ∷ Int
fixext8 = 0xd7 -- 11010111

fixext16 ∷ Int
fixext16 = 0xd8 -- 11011000

str8 ∷ Int
str8 = 0xd9 -- 11011001

str16 ∷ Int
str16 = 0xda -- 11011010

str32 ∷ Int
str32 = 0xdb -- 11011011

array16 ∷ Int
array16 = 0xdc -- 11011100

array32 ∷ Int
array32 = 0xdd -- 11011101

map16 ∷ Int
map16 = 0xde -- 11011110

map32 ∷ Int
map32 = 0xdf -- 11011111
