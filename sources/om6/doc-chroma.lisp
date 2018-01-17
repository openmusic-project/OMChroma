(in-package :om)

(init-chroma-ref-dir)

; evaluate this line to generate the documentation
; (chroma-classes-reference)

(defun chroma-classes-reference ()
  (gen-chroma-reference (gen-lib-entries (find-library "OMChroma")
                                         :exclude-packages '("Basic Classes" "Advanced Classes" "Classes")) 
                        *chroma-classes-entries* *chroma-ref-directory* 
                        :title "OMChroma Library Reference" 
                        :general-doc *chroma-general-doc* 
                        :doc-base *chroma-slots-doc* 
                        :appendix *chroma-appendix*)
  )

(setf *chroma-classes-entries* 
      '(("CSOUND" 
         (
          ("Additive" 
          (add-1 add-2 add-3 add-4 add-a1))
          ("Buzz Synthesis (Dynamic Spectrum Oscillators)"
           (buzz-1 buzz-2 bzfl-1))
          ("Frequency Modulation"
           (fm-1 fm-2))
          ("FOF (Formantic Waveform)"
           (fof-1 fof-2 fof-3 fof-4 fof-a1 fof-a4))
          ("FOG (Granular Formantic Waveform)"
           (fog-1))
          ("Karplus-Strong"
           (pluck-1 pluck-2))
          ("Random Amplitude Modulation"
           (ran-1 ranfl-1))
          ("Sampler (Reading from a sound file)"
           (smpl-1 smpl-2 smpl-a1 smpl-a2))
          ("Sampler (Using a deferred table)"
           (smpl-3 smpl-4))
          ("Sampler (Using flopper2 with a deferred table)"
           (smpl-5 smpl-6))
            ("Subtractive synthesis"
           (sub-1))
            ("Waveshaping"
           (wshp-1))
            ("Hybrid models"
           (snare-1))
         ))
))

(setf *chroma-slots-doc* 
      '(("e-dels" "Entry Delays [sec]")
        ("dist" "Noise distribution. Defines a white noise [0 = uniform, 1 = Gaussian...]")
        ("durs" "Durations [sec]")
        ("dur" "Duration for the whole event [sec]")
        ("amp" "Maximum Amplitude [lin, >0.0-1000.0 or dB <= 0.0]")
        ("bal" "Stereo Balance [-1->1, l-r, with cosine compensation, 0 = centre]")
        ("aenv"	"Amplitude Envelope [GEN]")
        ("afil"	"Audio file [name, with possible path, sound, string, pathname or GEN01]")
        ("atk"	"Attack time of the amplitude envelope [sec]")
        ("buf"	"Intended frequency of the noise (buffer) [sec]")
        ("bw"	"(Minimum) bandwith [Hz or % of centre freq]")
        ("bwenv" "Envelope of the bandwith [GEN]")
        ("bwmax" "Maximum bandwith [Hz or % of centre freq]")
        ("bzm" "Multiplier in the series of amp coefficients [flt]")
        ("bzmenv" "Envelope of the buzz multiplier [GEN]")
        ("bzh" "Highest harmonic/freq present in the buzz [%/Hz]")
        ("bzl" "Lowest harmonic/freq present in the buzz [%/Hz]")
        ("dec" "Decay time/% of the total dur of the amp envelope [sec/%]")
        ("f0" "(Minimum) fundamental frequency [Hz]")
        ("f0dur" "Duration of the envelope for f0 [sec, if 0, take global dur]")
        ("f0env" "Envelope of the f0 [GEN]")
        ("f0jta" "Jitter's amp of the fundamental frequency [% of f0]")
        ("f0max" "Maximum fundamental frequency [Hz]")
        ("fdev" "(Minimum) frequency deviation [semitones]")
        ("fenv" "Envelope of the frequency deviation [GEN]")
        ("fmod" "Modulating frequency [Hz]")
        ("freq" "(Minimum) current frequency [Hz]")
        ("fqenv" "Envelope of the current frequency [Hz]")
        ("fqjta" "Jietter's amp of the current frequency [% of freq]")
        ("fqmax" "Maximum current frequency [Hz]")
        ("ienv" "Envelope for the index [GEN]")
        ("imax" "Maximum index (of freq modulation) [flt]")
        ("imin" "Minimum index (of freq modulation) [flt]")
        ("inha" "Amplitude of the pseudo-inharmonic tone [lin, >0.0-1000.0 or dB <= 0.0]")
        ("jta" "Amplitude of the jitter/random source [%]")
        ("jtf" "(Centre) frequency of the jitter [%/Hz]")
        ("jtv" "Jitter/vibrato panpot [0-1, 0=all vib, 1=all jit]")
        ("lpbeg" "Starting loop point [%/samples]")
        ("lpend" "Ending loop point [%/samples]")
        ("meth" "Method of natural decay [1, 2, 3, 4, 5, 6]")
        ("mode" "Formant freq mode [0=no gliss within each grain]")
        ("n1" "N1 (for freq modulation) [flt]")
        ("n2" "N2 (for freq modulation) [flt]")
        ("oct" "(Minimum) octaviation factor of the fof [flt >=0.0]")
        ("octenv" "Envelope of the octaviation factor of the fof [GEN]")
        ("octmax" "Maximum octaviation factor of the fof [flt >=0.0]")
        ("par1" "Optional parametre required by some synthesis techniques [see class]")
        ("par2" "Optional parametre required by some synthesis techniques [see class]")
        ("pdur" "Portamento duration [sec]")
        ("penv" "Envelope of the portamento duration [GEN]")
        ("pflg" "Portamento flag [0=gliss+spectral effect / 1=gliss+spectral sweep]")
        ("phs" "Starting phase [%]")
        ("plow" "Minimum (lowest) portamento [semitones]")
        ("pup" "Maximum (highest) portamento [semitones]")
        ("ranfun" "Function number of the random source [GEN]")
        ("scal" "Spectral scaler for the audio freq [mlt]")
        ("skip" "(Minimum) starting point when reading an audio file [sec]")
        ("skpenv" "Envelope of the starting point when reading an audio file [GEN]")
        ("skpmax" "Maximum starting point when reading an audio file [sec]")
        ("spd" "(Minimum) speed (rate at which successive grains progress through the stored function table) [mlt, 1=same as original]")
        ("spdenv" "Envelope of the speed [GEN]")
        ("spdmax" "Maximum speed (rate at which successive grains progress through the stored function table) [mlt, 1=same as original]")
        ("tra" "Tremolo amp [%]")
        ("trf" "Tremolo freq [Hz]")
        ("xpf" "Transposition factor [mlt, 1=same as orig, <0.0 = reverse reading]")
        ("vfq" "Vibrato frequency [Hz]")
        ("wdur" "(Minimum) total duration of the FOF/grain [sec]")
        ("wdurenv" "Envelope of the total duration of the FOF/grain [sec]")
        ("wdurmax" "Maximum total duration of the FOF/grain [sec]")
        ("win" "(Minimum) excitation (rise/tex) time of the local attack (FOF/grain) [sec]")
        ("winenv" "Envelope of the excitation (rise/tex) time of the local attack (FOF/grain) [GEN]")
        ("winmax" "Maximum excitation (rise/tex) time of the local attack (FOF/grain) [sec]")
        ("wout" "(Minimum) decay time of the local decay (FOF/grain) [sec]")
        ("woutenv" "Envelope of the decay time of the local decay (FOF/grain) [GEN]")
        ("woutmax" "Maximum decay time of the local decay (FOF/grain) [sec]")
        ("wrap" "Wrap flag: 0=locations beyond EOF produce silence, <>0=wrap from beg of file")

;GLOBAL SLOTS
        ("action-time" "Start time of the whole event [sec]")
        ("numcols" "Number of components for the event [int]")
        ("parsing-fun" "A processing function applied to each component when the event is being evaluated [lambda/fun-name]")
        ))


; DEFAULT GEN FUNCTIONS FOR CSOUND"
(setf *chroma-appendix*
      (list "Csound Tables Conventions"
            '((1 "AUDIO (default size: 65537 points)")
              (2 "VIBRATO OR MODULATING (default size: 65537 points)")
              (3 "TREMOLO (default size: 65537 points)")
              (4 "LARGE NON-INTERPOLATING SINE (default size: 16777216 points)")
              (5 "LARGE COSINE (BUZZ, default size: 65537 points)")
              (6 "ASCENDING LINEAR SEGMENT FOR GLISSANDOS (0->1)")
              (7 "TRIANGLE FUNCTION")
              (8 "STRAIGHT LINE = 1")
              (10 "TRANSFER FUNCTION FOR WAVESHAPING")
              (11 "noise-modulated sine wave")
              (12 "sine wave with only one high partial (10th)")
              (13 "pseudo-inharmonic spectrum made of high partials")
              (19 "LARGE SIGMOID RISE/DECAY (1/2 COSINE, 65536 points)")
              (20 "INTERPOLATING ASCENDING SIGMOID RISE (1/2 COSINE, 65537 points)")
              (21 "INTERPOLATING DESCENDING SIGMOID RISE (1/2 COSINE, 65537 points)")
              (22 "SINE-BASED BELL SHAPE (SIN FROM -90 TO 270, 65537 points)")
              (23 "slowly descending exponential envelope")
              (24 "rapidly descending exponential envelope")
              (31 "AUDIO FILE (granular synthesis)")
              (32 "SHORT AUDIO FILE (granular synthesis)")
              )))


(setf *chroma-general-doc* 
" WELCOME TO THE ON-LINE DOCUMENTATION OF OMCHROMA
  LAST UPDATE: April 29, 2009, ms

 NAMING CONVENTIONS FOR THE VALUES OF THE SLOTS
 % is always expressed between 0.0 and 1.0 (not 0 and 100)
 int = integer number
 flt = floating point number
 mlt = multiplier of a reference value
 GEN = csound GEN function object or a pointer (int) to it

"
)

;(member "myslot" (mapcar 'name (get-all-initargs-of-class 'myclass)))



