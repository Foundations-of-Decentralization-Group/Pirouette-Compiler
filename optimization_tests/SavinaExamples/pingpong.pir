{- Based on https://github.com/shamsimam/savina/blob/master/src/main/scala/edu/rice/habanero/benchmarks/pingpong/PingPongHabaneroActorBenchmark.scala -}

pingpong := fun (Pinger.pingCount, Ponger.pongCount) ->
  if Pinger.(pingCount>0) then
    Pinger[PING] ~> Ponger;
    let Ponger.newPongCount := Ponger.(pongCount + 1); in

    {- debug print -}
    {-
    let Ponger._ := Ponger.print_string Ponger."pong number "; in
    let Ponger._ := Ponger.print_int Ponger.newPongCount; in
    let Ponger._ := Ponger.print_newline Ponger.(); in
    -}

    Ponger[PONG] ~> Pinger;
    let Pinger.newPingCount := Pinger.(pingCount - 1); in
    pingpong (Pinger.newPingCount, Ponger.newPongCount)
  else
    Pinger[STOP] ~> Ponger;
    Pinger.()
;

main :=
  {- to match N=40000 in PingPongConfig.java from original -}
  pingpong (Pinger.40000, Ponger.0)
;
