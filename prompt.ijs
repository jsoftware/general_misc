NB. prompt

NB. =========================================================
NB.*prompt v prompt for input
NB.
NB. prompts for input, optionally with a default result
NB.
NB. form: [default] prompt prompt_text
NB.
NB. examples:
NB.    prompt 'start date: '
NB.    '2001 5 23' prompt 'start date: '
NB.
NB. Notes:
NB.  - the default is only available in JQt
NB.  - this will not work in a script
prompt=: 3 : 0
'' prompt y
:
if. IFQT do.
  wd 'sm prompt ',y,x
  inp=. 1!:1 ] 1
  len=. #y
  (len * y -: len {. inp) }. inp
else.
  y 1!:2 [ 4
  1!:1 ] 1
end.
)