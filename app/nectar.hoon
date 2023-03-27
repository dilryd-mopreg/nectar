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
      perms=(map table-name:n permission-level:n)
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
      ~(. da table-sub bowl -:!>(*result:da) -:!>(*from:da))
    du-pub
      =/  du  (du shared-table ,[%track @ @ ~])
      ~(. du table-pub bowl -:!>(*result:du))
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
    ?>  =(our src):bowl  ::  TODO allow edits from others?? :O
    =/  poke  !<(query-poke:n vase)
    ::  use this only for stateful queries
    ::
    ~|  "nectar: query pokes are only for stateful queries!"
    ?>  ?=  $?  %update        %insert
                %delete        %add-table
                %rename-table  %drop-table
                %update-rows
            ==
        -.query.poke
    ::  if we are deleting or renaming a table that people track,
    ::  ....do something about that
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
          ?(%add-table %drop-table %rename-table)
        `table-pub
      ==
    [cards this]
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
      ::  TODO if we're already tracking someone, stop tracking them here!
      ::  TODO *kick* anyone tracking us!
      ::  =^  cards  table-pub
      ::    (give:du-pub [%track p -.tag.q ~] [%gone-top-level-tag ~])
      :-  (surf:da-sub source.q %nectar [%track [- + ~]:table-name.q])^~
      this(tracking.state (~(put by tracking.state) table-name.q source.q))
    ::
        %stop
      ::  TODO stop tracking a solid-state sub!
      =,  track
      `this(tracking.state (~(del by tracking.state) table-name.q))
    ==
  ::
  ::  SSS pokes
  ::
      %sss-shared-table
    =^  cards  table-sub
      (apply:da-sub !<(into:da-sub (fled vase)))
    [cards this]
  ::
      %sss-on-rock
    =/  msg  !<(from:da-sub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      =/  =app:n    -.+.-.msg
      =/  =label:n  -.+.+.-.msg
      ::  make sure we are actually tracking this table?
      ::  ?.  =(src.msg (~(gut by tracking.state) [app label] our.bowl))
      ::    `this  ::  TODO ignore for now, but crash in future when we can leave
      ~&  >>  "got on-rock for table {<[app label]>}"
      `this
    ==
  ::
      %sss-to-pub
    =/  msg  !<(into:du-pub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      =/  =app:n    -.+.-.msg
      =/  =label:n  -.+.+.-.msg
      =/  perm  (~(gut by perms.state) [app label] [%private ~])
      ::  only allow permitted subscribers
      ?.  ?|  ?=(%public -.perm)
              &(?=(%set -.perm) (~(has in +.perm) src.bowl))
          ==
        `this
      ::  separately from permissions, ignore subscribers
      ::  to tables that we ourselves are trackers for. this
      ::  is a choice that can be edited if desired, but if so,
      ::  note that rocks/waves we receive do not trigger us to
      ::  send out ones ourselves.
      ?:  !=(our.bowl (~(gut by tracking.state) [app label] our.bowl))
        `this
      =^  cards  table-pub
        (apply:du-pub msg)
      [cards this]
    ==
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ::  use this for stateless queries
  ::
  ?+    path  [~ ~]
      [%x %query @ ^]
    =/  app=@tas    i.t.t.path
    =/  label=@tas  i.t.t.t.path
    ::  apply params to axes
    =/  params=(list @)  t.t.t.t.path
    =/  proc=stored-procedure:n
      (~(got bi stored-procedures.state) app label)
    ::  inject variable params into query
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
    ~&  >  "your query: "
    ~&  >  q.proc
    ``noun+!>(`(list row:n)`-:(~(q db:n database.state) app q.proc))
  ::
      [%x %jammed-query @ @ ~]
    =/  app=@tas  i.t.t.path
    =/  =query:n  ;;(query:n (cue i.t.t.t.path))
    ::  perform query and return result
    ~&  >  "your query: "
    ~&  >  query
    ``noun+!>(`(list row:n)`-:(~(q db:n database.state) app query))
  ==
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