/-  subgraph
/+  verb, dbug, default-agent, io=agentio,
    g=social-graph, *mip, *sss
|%
::
::  %social-graph agent state
::
+$  state-2
  $:  %2
      graph=social-graph:g
      perms=(map [app:g tag:g] permission-level:g)
      tracking=(map [app:g tag:g] ship)
  ==
+$  card  card:agent:gall
::
::  scry paths
::
::  /controller/[app]/[tag]  <-  returns @p of who we source a tag from
::  /nodes/[app]/[from-node]/[tag]  <-  returns (set node)
::  /nodeset/[app]/[tag]            <-  returns the nodeset at app+tag
::  /tags/[from-node]/[to-node]     <-  returns (set tag)
::  /app-tags/[app]                 <-  returns (set tag)
::  /app/[app]                      <-  returns (map tag nodeset)
::  /has-tag/[app]/[from-node]/[to-node]/[tag]        <-  returns loobean
::  /bidirectional/[app]/[from-node]/[to-node]/[tag]  <-  returns loobean
--
::
^-  agent:gall
::  %+  verb  &
%-  agent:dbug
::  SSS declarations
=/  subgraph-sub  (mk-subs subgraph ,[%track @ @ ~])
=/  subgraph-pub  (mk-pubs subgraph ,[%track @ @ ~])
::
=|  state=state-2
=<
|_  =bowl:gall
+*  this  .
    hc    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    da-sub
      =/  da  (da subgraph ,[%track @ @ ~])
      (da subgraph-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
    du-pub
      =/  du  (du subgraph ,[%track @ @ ~])
      (du subgraph-pub bowl -:!>(*result:du))
::
++  on-init  `this(state *state-2)
::
++  on-save  !>([state subgraph-sub subgraph-pub])
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  ?:  =(%0 -.q.vase)
    on-init
  ?:  =(%1 -.-.q.vase)
    on-init
  =/  old  !<([=state-2 =_subgraph-sub =_subgraph-pub] vase)
  :-  ~
  %=  this
    state         state-2.old
    subgraph-sub  subgraph-sub.old
    subgraph-pub  subgraph-pub.old
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %social-graph-edit
    ?>  =(our src):bowl
    =/  =edit:g  !<(edit:g vase)
    ::  !! need this info in bowl for perms !!
    =/  =app:g  p.edit
    =/  =tag:g  ::  this is so annoying, type refinement issue
      ?-  -.q.edit
        %set-perms                        tag.q.edit
        ?(%add-tag %del-tag)              tag.q.edit
        ?(%nuke-tag %nuke-top-level-tag)  tag.q.edit
      ==
    ::  tag cannot be empty
    ?:  ?=(~ tag)
      ~|("social-graph: tag cannot be empty!" !!)
    ?:  ?=(%set-perms -.q.edit)
      ::  change permissions for a subgraph publication
      =/  paths  [%track app i.tag ~]^~
      =.  subgraph-pub
        ?-  level.q.edit
          %public   (public:du-pub paths)
          %private  (secret:du-pub paths)
            %only-tagged
          %+  allow:du-pub
            ::  src.bowl must appear in nodeset under top-level tag
            =-  %+  murn  ~(tap in -)
                |=  =node:g
                ?.  ?=(%ship -.node)  ~
                `+.node
            (nodeset-to-set:g (~(get-nodeset sg:g graph.state) app i.tag^~))
          paths
        ==
      `this(perms.state (~(put by perms.state) [app i.tag^~] level.q.edit))
    ::  reject edits to graph if we are tracking
    ::  someone else's on the given app+tag
    ?:  (~(has by tracking.state) [app i.tag^~])
      ~|("social-graph: can't edit a tracked tag!" !!)
    =^  wave  graph.state
      ?-  -.q.edit
          %add-tag
        :-  [%new-edge tag from.q.edit to.q.edit]
        (~(add-tag sg:g graph.state) from.q.edit to.q.edit app tag)
          %del-tag
        :-  [%gone-edge tag from.q.edit to.q.edit]
        (~(del-tag sg:g graph.state) from.q.edit to.q.edit app tag)
          %nuke-tag
        :-  [%gone-tag tag ~]
        (~(nuke-tag sg:g graph.state) app tag)
          %nuke-top-level-tag
        :-  [%gone-top-level-tag ~]
        (~(nuke-top-level-tag sg:g graph.state) app i.tag)
      ==
    ::  allow/block edited ships on top-level tags, if subgraph is
    ::  permissioned to %only-tagged
    =?    subgraph-pub
        ?=(%only-tagged (~(gut by perms.state) [app i.tag^~] %private))
      ?.  ?=(?(%add-tag %del-tag) -.q.edit)  subgraph-pub
      =/  new=(set @p)
        ?:  &(?=(%ship -.from.q.edit) ?=(%ship -.to.q.edit))
          (silt ~[+.from +.to]:q.edit)
        ?:  ?=(%ship -.from.q.edit)
          [+.from.q.edit ~ ~]
        ?:  ?=(%ship -.to.q.edit)
          [+.to.q.edit ~ ~]
        ~
      ::  only remove the ship(s) that were untagged if they no longer
      ::  appear in the top-level-tag nodeset!
      =?    new
          ?=(%del-tag -.q.edit)
        %-  silt
        %+  skip  ~(tap in new)
        |=  p=@p
        (in-nodeset:g ship+p (~(get-nodeset sg:g graph.state) app i.tag^~))
      %+  perm:du-pub  [%track app i.tag ~]^~
      |=  old=(unit (set @p))
      ?-    -.q.edit
          %add-tag
        ?~(old `new `(~(uni in u.old) new))
          %del-tag
        ?~(old `~ `(~(dif in u.old) new))
      ==
    ::  hand out update to subscribers on this app and (top-level) tag
    =^  cards  subgraph-pub
      (give:du-pub [%track app i.tag ~] wave)
    [cards this]
  ::
      %social-graph-track
    ?>  =(our src):bowl
    =/  =track:g  !<(track:g vase)
    =,  track
    ::  don't track yourself..
    ?:  =(our.bowl source.q)
      ~|("social-graph: don't track yourself!" !!)
    ?:  ?=(~ tag.q)
      ~|("social-graph: tracked tag cannot be empty!" !!)
    =/  path  [%track p -.tag.q ~]
    ?-    -.q.track
        %start
      ::  destroy our local representation of this top-level tag,
      ::  to prepare for synchronization with remote.
      =.  graph.state
        (~(nuke-top-level-tag sg:g graph.state) p -.tag.q)
      ::  if we're already tracking someone, stop tracking them here!
      =/  prev  (~(get by tracking.state) [p -.tag.q^~])
      =?    subgraph-sub
          ?=(^ prev)
        (quit:da-sub u.prev %social-graph path)
      ::  kill our path if we were serving this content previously
      =.  subgraph-pub  (kill:du-pub path^~)
      ::  start watching the chosen publisher
      =^  cards  subgraph-sub
        (surf:da-sub source.q %social-graph path)
      :-  cards
      this(tracking.state (~(put by tracking.state) [p -.tag.q^~] source.q))
    ::
        %stop
      ::  when we stop tracking remote, destroy our local representation!
      ::  we do this so that future publications we might host ourselves
      ::  are not out of sync with our old local representation of a remote.
      :-  ~
      %=  this
        tracking.state  (~(del by tracking.state) [p -.tag.q^~])
        subgraph-sub    (quit:da-sub source.q %social-graph path)
        graph.state     (~(nuke-top-level-tag sg:g graph.state) p -.tag.q)
      ==
    ==
  ::
  ::  SSS pokes
  ::
      %sss-subgraph
    =^  cards  subgraph-sub
      (apply:da-sub !<(into:da-sub (fled vase)))
    [cards this]
  ::
      %sss-surf-fail
    =/  msg  !<(fail:da-sub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      ~&  >>>  "social-graph: not allowed to track {<msg>}!"
      `this(tracking.state (~(del by tracking.state) [- -.+^~]:+.-.msg))
    ==
  ::
      %sss-to-pub
    =/  msg  !<(into:du-pub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      =/  =app:g  `@tas`-.+.-.msg
      =/  =tag:g  `path`+.+.-.msg
      ::  crash here to disable tracking on private subgraphs
      =/  perm=permission-level:g  (~(gut by perms.state) +.-.msg %private)
      ?>  ?|  ?=(%public perm)
              ?&  ?=(%only-tagged perm)
                  ::  src.bowl must appear in nodeset under top-level tag
                  %+  in-nodeset:g  ship+src.bowl
                  (~(get-nodeset sg:g graph.state) app tag)
          ==  ==
      ::  separately from direct permissioning, don't let people track
      ::  subgraphs that we ourselves are watchers for. this is because
      ::  we won't be issuing a publication, so it would mislead them.
      ?<  (~(has by tracking.state) [app tag])
      =^  cards  subgraph-pub
        (apply:du-pub msg)
      [cards this]
    ==
  ::
      %sss-on-rock
    =/  msg  !<(from:da-sub (fled vase))
    ?-    -.msg
        [%track @ @ ~]
      =/  =app:g  `@tas`-.+.-.msg
      =/  =tag:g  `path`+.+.-.msg
      ::  quit sub if sender not in our tracking map
      ?.  =(src.msg (~(gut by tracking.state) [app tag] our.bowl))
        :_  this  :_  ~
        %+  ~(poke pass:io /self)
          [our.bowl %social-graph]
        social-graph-track+!>(`track:g`[app %stop src.msg tag])
      =.  graph.state
        ?~  wave.msg
          ::  if no wave, use rock in msg as setpoint
          =/  l=(list [=tag:g =nodeset:g])  ~(tap by rock.msg)
          |-
          ?~  l  graph.state
          =-  $(l t.l, graph.state -)
          %-  ~(replace-nodeset sg:g graph.state)
          [nodeset.i.l app tag.i.l]
        ::  integrate wave into our local graph
        ?-    -.u.wave.msg
            %new-edge
          %-  ~(add-tag sg:g graph.state)
          [from.u.wave.msg to.u.wave.msg app tag.u.wave.msg]
            %gone-edge
          %-  ~(del-tag sg:g graph.state)
          [from.u.wave.msg to.u.wave.msg app tag.u.wave.msg]
            %gone-tag
          (~(nuke-tag sg:g graph.state) app tag.u.wave.msg)
            %gone-top-level-tag
          (~(nuke-top-level-tag sg:g graph:state) app -.tag)
        ==
      `this
    ==
  ==
::
++  on-peek   handle-scry:hc
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign  `this
  ?+    wire   `this
      [~ %sss %on-rock @ @ @ %track @ @ ~]
    =.  subgraph-sub  (chit:da-sub |3:wire sign)
    `this
  ::
      [~ %sss %scry-request @ @ @ %track @ @ ~]
    ~&  >>>  "social-graph: removed from {<|3:wire>}"
    =^  cards  subgraph-sub  (tell:da-sub |3:wire sign)
    =/  =app:g  -:|7:wire
    =/  top=@   -:|8:wire
    :-  cards
    %=  this
      tracking.state  (~(del by tracking.state) [app top^~])
      graph.state     (~(nuke-top-level-tag sg:g graph.state) app top)
    ==
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
::  start helper core
|_  =bowl:gall
++  handle-scry
  ::  /controller/[app]/[tag]  <-  returns @p of who we source a tag from
  ::  /nodes/[app]/[from-node]/[tag]  <-  returns (set node)
  ::  /nodeset/[app]/[tag]            <-  returns the nodeset at app+tag
  ::  /tags/[from-node]/[to-node]     <-  returns (set tag)
  ::  /app-tags/[app]                 <-  returns (set tag)
  ::  /app/[app]                      <-  returns (map tag nodeset)
  ::  /has-tag/[app]/[from-node]/[to-node]/[tag]        <-  returns loobean
  ::  /bidirectional/[app]/[from-node]/[to-node]/[tag]  <-  returns loobean
  |=  =path
  ^-  (unit (unit cage))
  ?:  |(?=(~ path) !=(%x i.path) ?=(~ t.path))  !!
  =/  path  t.path
  ?:  ?=([%is-installed ~] path)
    ``json+!>(`json`[%b &])
  :^  ~  ~  %social-graph-result
  !>  ^-  graph-result:g
  ?+    path
    ~|("unexpected scry into {<dap.bowl>} on path {<path>}" !!)
  ::
  ::  /controller/[app]/[tag]
  ::  returns the ship who controls a given app+tag for us
  ::
      [%controller @ ^]
    =/  =app:g  `@tas`i.t.path
    =/  =tag:g  t.t.path
    ?~  tag  controller+our.bowl
    controller+(~(gut by tracking.state) [app i.tag^~] our.bowl)
  ::
  ::  /nodeset/[app]/[tag]
  ::  returns the full nodeset (jug node node) in given app+tag
  ::
      [%nodeset @ ^]
    =/  =app:g  `@tas`i.t.path
    =/  =tag:g  t.t.path
    nodeset+(~(get-nodeset sg:g graph.state) app tag)
  ::
  ::  /nodes/[app]/[from-node]/[tag]
  ::  returns a set of all nodes connected to given node in given app+tag
  ::
      ?([%nodes @ ?(%ship %address) @ ^] [%nodes @ %entity @ @ ^])
    =/  =app:g  `@tas`i.t.path
    =/  =node:g
      =+  i.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.path)]
        %entity   [- [`@tas`i `@t`i.t]:t.t.t.path]
      ==
    =/  =tag:g  t.t.t.t.path
    nodes+(~(get-nodes sg:g graph.state) node app `tag)
  ::
  ::  /nodes/[app]/[from-node]
  ::  returns a set of all nodes connected to given node in given app
  ::
      ?([%nodes @ ?(%ship %address) @ ~] [%nodes @ %entity @ @ ~])
    =/  =app:g  `@tas`i.t.path
    =/  =node:g
      =+  i.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.path)]
        %entity   [- [`@tas`i `@t`i.t]:t.t.path]
      ==
    nodes+(~(get-nodes sg:g graph.state) node app ~)
  ::
  ::  /tags/[app]/[from-node]/[to-node]
  ::  returns a set of all tags on edge between two nodes, in given app
  ::
      $%  [%tags @ ?(%ship %address) @ ?(%ship %address) @ ~]
          [%tags @ ?(%ship %address) @ %entity @ @ ~]
      ==
    =/  =app:g  `@tas`i.t.path
    =/  n1=node:g
      =+  i.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.path)]
      ==
    =/  n2=node:g
      =+  i.t.t.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.t.t.path)]
        %entity   [%entity `@tas`i `@t`i.t]:t.t.t.t.path
      ==
    =-  tags+?~(- ~ (~(get ju u.-) app))
    (~(get-edge sg:g graph.state) n1 n2)
  ::
      $%  [%tags @ %entity @ @ ?(%ship %address) @ ~]
          [%tags @ %entity @ @ %entity @ @ ~]
      ==
    =/  =app:g  `@tas`i.t.path
    =/  n1=node:g
      [i `@tas`i.t `@t`i.t.t]:t.t.path
    =/  n2=node:g
      =+  i.t.t.t.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.t.t.t.path)]
        %entity   [%entity `@tas`i `@t`i.t]:t.t.t.t.t.path
      ==
    =-  tags+?~(- ~ (~(get ju u.-) app))
    (~(get-edge sg:g graph.state) n1 n2)
  ::
  ::  /app-tags/[app]
  ::  returns set of all tags stored by given app
  ::
      [%app-tags @ ~]
    =/  =app:g  `@tas`i.t.path
    app-tags+(~(get-app-tags sg:g graph.state) app)
  ::
  ::  /app/[app]
  ::  returns full mapping of all information held in given app
  ::
      [%app @ ~]
    =/  =app:g  `@tas`i.t.path
    app+(~(get-app sg:g graph.state) app)
  ::
  ::  /has-tag/[app]/[from-node]/[to-node]/[tag]
  ::  returns true if tag exists on given edge, false otherwise
  ::
      $%  [%has-tag @ ?(%ship %address) @ ?(%ship %address) @ ^]
          [%has-tag @ ?(%ship %address) @ %entity @ @ ^]
      ==
    =/  =app:g  `@tas`i.t.path
    =/  n1=node:g
      =+  i.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.path)]
      ==
    =/  n2=node:g
      =+  i.t.t.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.t.t.path)]
        %entity   [%entity `@tas`i `@t`i.t]:t.t.t.t.path
      ==
    =/  =tag:g
      ?-  i.t.t.t.t.path
        ?(%ship %address)  t.t.t.t.t.t.path
        %entity            t.t.t.t.t.t.t.path
      ==
    (~(has-tag sg:g graph.state) n1 n2 app tag)
  ::
      $%  [%has-tag @ %entity @ @ ?(%ship %address) @ ^]
          [%has-tag @ %entity @ @ %entity @ @ ^]
      ==
    =/  =app:g  `@tas`i.t.path
    =/  n1=node:g
      [i `@tas`i.t `@t`i.t.t]:t.t.path
    =/  n2=node:g
      =+  i.t.t.t.t.t.path
      ?-  -
        %ship     [- (slav %p i.t.t.t.t.t.t.path)]
        %address  [- (slav %ux i.t.t.t.t.t.t.path)]
        %entity   [%entity `@tas`i `@t`i.t]:t.t.t.t.t.path
      ==
    =/  =tag:g
      ?-  i.t.t.t.t.t.path
        ?(%ship %address)  t.t.t.t.t.t.t.path
        %entity            t.t.t.t.t.t.t.t.path
      ==
    (~(has-tag sg:g graph.state) n1 n2 app tag)
  ==
--