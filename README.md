# race-tmvar-async-exception

This code demonstrates an issue in GHC runtime where runtime stalls at
killThread and falls into high CPU.

## Summary of the issue

When you successfully reproduced it, you will see

* Reproduces with +RTS -Nx where x > 1
* Does NOT reproduce with +RTS -N1
* Program stalls after 'A' printed
* High CPU based on given -Nx
    * CPU won't be 100% if you gave x smaller than available hardware threads
      of your platform.
* Does NOT reproduce if TMVar is replaced by MVar

This program intentionally creates race condition between TMVar operation and
asynchronous exception.  In one side, putTMVar from external thread attempts to
unblock a thread blocked by takeTMVar.  On the other side, asynchronous
exception ThreadKilled thrown by external thread attempts to interrupt the same
blocked thread.

I guess when those operation happen at in parallel in SMP environment, GHC
runtime falls into high CPU.

## Reproduced platforms

* macOS High Sierra (10.13.4), MacBook Pro (Retina, 15-inch, Mid 2015)
    * GHC 8.4.2, base-4.11.1.0, stm-2.4.5.0
* Docker for Mac Version 18.03.1-ce-mac65 (24312) runs on the above machine.
    * GHC 8.4.2, base-4.11.1.0, stm-2.4.5.0
    * ghc-8.5.20180506

Others are unknown (not yet attempted).

## Usage

Use stack to build the app.

```console
$ git clone https://github.com/nshimaza/race-tmvar-async-exception.git
$ cd race-tmvar-async-exception
$ stack build
```
Run the executable with SMP enabled.

```console
$ .stack-work/install/x86_64-linux/nightly-2018-05-01/8.4.2/bin/race-tmvar-async-exception-exe +RTS -N
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 A^C^C
$
```

When the issue was successfully reproduced, the program stops at after `A`.
You may need to press control-C twice to get back to shell prompt.
The number of displayed values can vary time to time you run the app.
If all numbers from 1 to 1000 were printed, reproducing  was unsuccessful.

Here is an example when the issue was not reproduced.

```console
$ .stack-work/install/x86_64-linux/nightly-2018-05-01/8.4.2/bin/race-tmvar-async-exception-exe +RTS -N1
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
...
990 991 992 993 994 995 996 997 998 999 1000 $
```

### Tuning

Reproducing the issue highly depends on runtime execution timing.  The timing
was tuned via two of `threadDelay` found in the code but it doesn't guarantee
the right timing on your environment.  To ensure the app really creating
racy condition, add `v` option at the first command line argument.

```console
$ .stack-work/install/x86_64-linux/nightly-2018-05-01/8.4.2/bin/race-tmvar-async-exception-exe v
1 (471) 2 (412) 3 (403) 4 (427) 5 A^C^C
$
```

Each time the program attempts to reproduce, it prints `num (num)` where the
first number is sequence of the trial and the second number is actually killed
threads.  You must have killed threads something between 2 to 999.
If you got `Expect: normalCount non-0  Got: 0` all threads are killed before
it started to exit.  If you got `Expect: killedCount non-0  Got: 0` all threads
are normally exited before main thread started to kill them.  If you get one
of them for all attempts, your threadDelay is too small or too big.
