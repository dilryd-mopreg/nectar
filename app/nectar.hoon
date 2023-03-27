/-  shared-table
/+  verb, dbug, default-agent,
    io=agentio, n=nectar, *mip, *sss
|%
::
::  %nectar agent state
::
+$  state-1
  $:  %1
      =database:n
      tracking=(map table-name:n ship)
      stored-procedures=(mip app:n label:n stored-procedure:n)
  ==
+$  card  card:agent:gall
--
::
^-  agent:gall
%+  verb  &
%-  agent:dbug
::  SSS declarations
=/  table-sub  (mk-subs shared-table ,[%track @ @ ~])
=/  table-pub  (mk-pubs shared-table ,[%track @ @ ~])
::
=|  state=state-1
|_  =bowl:gall
+*  this  .
    hc    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    da-sub
      =/  da  (da shared-table ,[%track @ @ ~])
      (da table-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
    du-pub
      =/  du  (du shared-table ,[%track @ @ ~])
      (du table-pub bowl -:!>(*result:du))
::
++  on-init  `this(state *state-1)
::
++  on-save  !>([state table-sub table-pub])
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  ?:  =(%0 -.q.vase)
    on-init
  =/  old  !<([=state-1 =_table-sub =_table-pub] vase)
  :-  ~
  %=  this
    state      state-1.old
    table-sub  table-sub.old
    table-pub  table-pub.old
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %nectar-query
    ?>  =(our src):bowl  ::  TODO allow edits from others??
    =/  poke  !<(query-poke:n vase)
    ::  use this only for stateful queries on tables that WE control
    ::
    ?.  ?=  $?  %update        %insert
                %delete        %add-table
                %rename-table  %drop-table
                %update-rows
            ==
        -.query.poke
      ~|("nectar: query pokes are only for stateful queries!" !!)
    =.  database.state  +:(~(q db:n database.state) poke)
    =^  cards  table-pub
      ::  stupid ass type refinement is the reason for this structure
      ?-    -.query.poke
          %update
        (give:du-pub [%track app.poke table.query.poke ~] query.poke)
      ::
          ?(%insert %delete %update-rows)
        (give:du-pub [%track app.poke table.query.poke ~] query.poke)
      ::
          %add-table
        =.  table-pub
          (live:du-pub [%track app.poke name.query.poke ~]^~)
        (give:du-pub [%track app.poke name.query.poke ~] query.poke)
      ::  if we are deleting or renaming a table that people track,
      ::  kill the publication
          %drop-table
        `(kill:du-pub [%track app.poke name.query.poke ~]^~)
          %rename-table
        =.  table-pub
          (live:du-pub [%track app.poke new.query.poke ~]^~)
        `(kill:du-pub [%track app.poke old.query.poke ~]^~)
      ==
    [cards this]
  ::
      %nectar-set-perms
    ?>  =(our src):bowl
    =/  poke   !<(set-perms:n vase)
    =/  paths  [%track [- + ~]:p.poke]^~
    =.  table-pub
      ?-  -.q.poke
        %public   (public:du-pub paths)
        %private  (secret:du-pub paths)
        %set      (allow:du-pub ~(tap in +.q.poke) paths)
      ::
          %add
        %+  perm:du-pub  paths
        |=  old=(unit (set @p))
        ?~  old  `+.q.poke
        `(~(uni in u.old) +.q.poke)
          %del
        %+  perm:du-pub  paths
        |=  old=(unit (set @p))
        ?~  old  `+.q.poke
        `(~(dif in u.old) +.q.poke)
      ==
    `this
  ::
      %nectar-add-procedure
    ?>  =(our src):bowl
    =+  !<(procedure-poke:n vase)
    `this(stored-procedures.state (~(put bi stored-procedures.state) -))
  ::
      %nectar-track
    ?>  =(our src):bowl
    =/  =track:n  !<(track:n vase)
    ::  don't track yourself..
    ?:  =(our.bowl source.q.track)
      ~|("nectar: don't track yourself!" !!)
    ?-    -.q.track
        %start
      ::  destroy our local representation of this table,
      ::  to prepare for synchronization with remote
      =,  track
      =.  database.state
        +:(~(q db:n database.state) -.table-name.q [%drop-table +.table-name.q])
      ::  if we're already tracking someone, stop tracking them here!
      =?    table-sub
          (~(has by tracking.state) table-name.q)
        (quit:da-sub source.q %nectar [%track [- + ~]:table-name.q])
      ::  kill our path if we were serving this content previously
      =.  table-pub  (kill:du-pub [%track [- + ~]:table-name.q]^~)
      ::  start watching the chosen publisher
      =^  cards  table-sub
        (surf:da-sub source.q %nectar [%track [- + ~]:table-name.q])
      :-  cards
      this(tracking.state (~(put by tracking.state) [table-name source]:q))
    ::
        %stop
      =,  track
      :-  ~
      %=  this
        tracking.state  (~(del by tracking.state) table-name.q)
        table-sub  (quit:da-sub source.q %nectar [%track [- + ~]:table-name.q])
      ==
    ==
  ::
  ::  SSS pokes
  ::
      %sss-shared-table
    =^  cards  table-sub
      (apply:da-sub !<(into:da-sub (fled vase)))
    [cards this]
  ::
      %sss-surf-fail
    =/  msg  !<(fail:da-sub (fled vase))
    ~&  >>>  "not allowed to surf on {<msg>}!"
    `this
  ::
      %sss-on-rock
    =/  msg  !<(from:da-sub (fled vase))
    ?-  -.msg
      [%track @ @ ~]  `this
    ==
  ::
      %sss-to-pub
    =/  msg  !<(into:du-pub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      =^  cards  table-pub
        (apply:du-pub msg)
      [cards this]
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  |^
  ::  use this for stateless queries
  ::
  ?+    path  [~ ~]
      [%x %query @ ^]
    =/  =app:n    i.t.t.path
    =/  =label:n  i.t.t.t.path
    ::  apply params to axes
    =/  params=(list @)  t.t.t.t.path
    =/  proc=stored-procedure:n
      (~(got bi stored-procedures.state) app label)
    ::  inject variable params into query
    ::  TODO make this easier to use lol
    =.  q.proc
      |-
      ?~  params  q.proc
      ?~  p.proc  q.proc
      =/  val  (slav -.i.p.proc i.params)
      %=    $
          params  t.params
          p.proc  t.p.proc
          q.proc
        =-  !<(query:n [-:!>(*query:n) -])
        =<  q
        %+  slap  !>(`*`q.proc)
        :+  %cnts  ~[%&^1]
        ~[[p=~[[%& +.i.p.proc]] q=[%sand -.i.p.proc i.params]]]
      ==
    ::  perform query and return result
    ~&  >  "nectar query: {<q.proc>}"
    ``noun+(mix-tables app label q.proc)
  ::
      [%x %jammed-query @ @ @ ~]
    =/  =app:n    i.t.t.path
    =/  =label:n  i.t.t.t.path
    =/  =query:n  ;;(query:n (cue i.t.t.t.t.path))
    ::  perform query and return result
    ~&  >  "nectar query: {<query>}"
    ``noun+(mix-tables app label query)
  ==
  ::
  ++  mix-tables
    |=  [=app:n =label:n =query:n]
    ^-  vase
    ::  if query is on a table we *track*, temporarily insert that table
    ::  into our DB for this query. TODO consider whether there's a better
    ::  option here -- should we just replicate all table-rocks locally?
    =/  =database:n
      ?~  ship=(~(get by tracking.state) [app label])
        database.state
      %+  ~(put by database.state)  [app label]
      =+  [u.ship %nectar [%track app label ~]]
      +.+:(~(gut by read:da-sub) - ``*table:n)
    !>(`(list row:n)`-:(~(q db:n database) app query))
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign  `this
  %-  (slog u.p.sign)
  ?+    wire   `this
      [~ %sss %on-rock @ @ @ %track @ @ ~]
    =.  table-sub  (chit:da-sub |3:wire sign)
    `this
    ::
      [~ %sss %scry-request @ @ @ %track @ @ ~]
    =^  cards  table-sub  (tell:da-sub |3:wire sign)
    [cards this]
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+  wire  `this
    [~ %sss %behn @ @ @ %track @ @ ~]  [(behn:da-sub |3:wire) this]
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--