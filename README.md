# mancini

I wanted to practice transcribing music using Euterpea, and had
some Henry Mancini tunes around from when my kids took piano lessons.

Setting up my ubuntu laptop to produce sounds was somewhat of a chore
and I didn't record all the steps. However, I followed the instructions
on how to install Euterpea (http://www.euterpea.com/download-and-installation/#support) and ultimately succeeded.
I'm using `qsynth` and `qjackctl` for a synthesizer.

With that out of the way, I could start "composing". The process of
transcribing is pretty clumsy so far, but I learned most of what I know
about sheet music from wikipedea recently. To keep the left and right hands
of the piano score in sync, and to keep track of where I was, I resorted
to organizing the transcription by hand, page, and measure. That's fine to
get the raw notes into the program, but it's a terrible way to represent
the structure of the tune when I have the features of Euterpea at my disposal.

I'm committing my original transcription for safety and reference. I would
like to refactor the structure to take advantage of themes and variations.
As yet I also don't honor some notation regarding dynamics or articulation.

Some day I'll do some project housekeeping. In the meantime, the way I hear
the music is as follows:

    clone Euterpea2
    change to Euterpea2 directory
    cabal repl
    :l ../mancini/baby-elephant-walk.hs
    main

