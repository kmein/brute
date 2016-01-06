# brute
A simple brute-force program

## Synopsis
`brute *OPTIONS* [-w WORD]`

## Description
`brute` brute is a simple program for brute-forcing strings written in Haskell.

It all began when I was challenged by a colleague to crack a
self-password-protected PDF file. The program I initially used was quite slow
and did not really have that many options for narrowing the search criteria.
Therefore I decided to write a very configurable brute-forcer.
The choice of Haskell arose from the fact that I wanted to challenge myself
to get back into Haskell after a long programming break. Also, it did kind of
impose another challenge.

Now, back to the program itself: As already mentioned, the key point for `brute`
is configurability. This configurability is achieved by many combinable
command line flags which control, whether, for example, numbers should be
included in the brute-force alphabet. There is also an option for adding a
user-defined list of characters. Given those alphabet switches, a press of the
`<ENTER>` key starts the search. The latter comes in two modes:

  * *timing mode*, which is invoked by the `-w *WORD*` switch and then
    runs a search for `*WORD*` using the specified alphabet. When the word
    is eventually found, the elapsed time in seconds will be displayed below.
    This mode can be used for testing password strength.
  * *list mode*, which simply lists all possible combinations of characters
    from the specified alphabet. This *might* be good for brute-force cracking
    files.

Outputs from both modes can of course be redirected to a file where they might
serve as a dictionary for a dictionary attack. (By the way, there is no
dictionary brute force mode in `brute` because it just lists words. For a file
like a dictionary, that would be (on Unix) `cat`s job - or for timing `time`s.
Windows users can use `type` instead of `cat`. For `time` there is, as far as I
know, no alternative.)

### Building `brute`
`brute` requires the following programs for building

  * `cabal` (I used 1.22.5)
  * `ghc` (I used 7.10.3, but any version above 7.6 would be OK)

Both of them are included in the Haskell Platform.

To build `brute`, just execute the following command in the project directory
(the directory with the `brute.cabal` file in it):

```
$ cabal build
```

Optionally, the haddock documentation can be built with

```
$ cabal haddock
```

The executable can then be found at `build/brute/brute`.

### Installing `brute`

### Using `brute`

## Options


