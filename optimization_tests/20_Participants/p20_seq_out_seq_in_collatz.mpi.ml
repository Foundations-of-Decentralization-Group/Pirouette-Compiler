let loc_to_rank =
  function
  | "P1" -> 0
  | "P10" -> 1
  | "P11" -> 2
  | "P12" -> 3
  | "P13" -> 4
  | "P14" -> 5
  | "P15" -> 6
  | "P16" -> 7
  | "P17" -> 8
  | "P18" -> 9
  | "P19" -> 10
  | "P2" -> 11
  | "P20" -> 12
  | "P3" -> 13
  | "P4" -> 14
  | "P5" -> 15
  | "P6" -> 16
  | "P7" -> 17
  | "P8" -> 18
  | "P9" -> 19
  | _ -> failwith "Runtime Error: Unknown location"
let _ = Mpi.barrier Mpi.comm_world
let _ =
  match Mpi.comm_rank Mpi.comm_world with
  | 0 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_1 = () in
      let rec _unit_2 = () in
      let rec _unit_3 = () in
      let rec _unit_4 = () in
      let rec _unit_5 = () in
      let rec _unit_6 = () in
      let rec _unit_7 = () in
      let rec _unit_8 = () in
      let rec _unit_9 = () in
      let rec _unit_10 = () in
      let rec _unit_11 = () in
      let rec _unit_12 = () in
      let rec _unit_13 = () in
      let rec _unit_14 = () in
      let rec _unit_15 = () in
      let rec _unit_16 = () in
      let rec _unit_17 = () in
      let rec _unit_18 = () in
      let rec _unit_19 = () in
      let rec loop iter =
        if iter > 0
        then
          (Mpi.send "L" (loc_to_rank "P2") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P3") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P4") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P5") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P6") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P7") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P8") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P9") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P10") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P11") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P12") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P13") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P14") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P15") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P16") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P17") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P18") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P19") 0 Mpi.comm_world;
           Mpi.send "L" (loc_to_rank "P20") 0 Mpi.comm_world;
           (let rec reply_P2 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P2") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P3 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P3") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P4 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P4") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P5 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P5") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P6 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P6") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P7 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P7") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P8 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P8") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P9 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P9") Mpi.any_tag Mpi.comm_world) 0 in
            let rec reply_P10 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P10") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P11 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P11") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P12 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P12") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P13 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P13") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P14 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P14") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P15 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P15") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P16 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P16") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P17 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P17") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P18 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P18") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P19 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P19") Mpi.any_tag Mpi.comm_world)
                0 in
            let rec reply_P20 =
              Marshal.from_string
                (Mpi.receive (loc_to_rank "P20") Mpi.any_tag Mpi.comm_world)
                0 in
            loop (iter - 1)))
        else
          (Mpi.send "R" (loc_to_rank "P2") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P3") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P4") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P5") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P6") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P7") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P8") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P9") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P10") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P11") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P12") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P13") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P14") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P15") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P16") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P17") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P18") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P19") 0 Mpi.comm_world;
           Mpi.send "R" (loc_to_rank "P20") 0 Mpi.comm_world;
           ()) in
      let rec start_time = gettimeofday () in
      let rec _unit_20 = loop 100 in
      let rec end_time = gettimeofday () in
      let rec time_diff = (sub_float end_time) start_time in
      print_float time_diff
  | 1 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_21 = () in
      let rec _unit_22 = () in
      let rec _unit_23 = () in
      let rec _unit_24 = () in
      let rec _unit_25 = () in
      let rec _unit_26 = () in
      let rec _unit_27 = () in
      let rec _unit_28 = () in
      let rec _unit_29 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_30 = () in
      let rec _unit_31 = () in
      let rec _unit_32 = () in
      let rec _unit_33 = () in
      let rec _unit_34 = () in
      let rec _unit_35 = () in
      let rec _unit_36 = () in
      let rec _unit_37 = () in
      let rec _unit_38 = () in
      let rec _unit_39 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_41 =
              let val_40 = result in
              Mpi.send (Marshal.to_string val_40 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_42 = loop () in ()
  | 2 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_43 = () in
      let rec _unit_44 = () in
      let rec _unit_45 = () in
      let rec _unit_46 = () in
      let rec _unit_47 = () in
      let rec _unit_48 = () in
      let rec _unit_49 = () in
      let rec _unit_50 = () in
      let rec _unit_51 = () in
      let rec _unit_52 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_53 = () in
      let rec _unit_54 = () in
      let rec _unit_55 = () in
      let rec _unit_56 = () in
      let rec _unit_57 = () in
      let rec _unit_58 = () in
      let rec _unit_59 = () in
      let rec _unit_60 = () in
      let rec _unit_61 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_63 =
              let val_62 = result in
              Mpi.send (Marshal.to_string val_62 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_64 = loop () in ()
  | 3 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_65 = () in
      let rec _unit_66 = () in
      let rec _unit_67 = () in
      let rec _unit_68 = () in
      let rec _unit_69 = () in
      let rec _unit_70 = () in
      let rec _unit_71 = () in
      let rec _unit_72 = () in
      let rec _unit_73 = () in
      let rec _unit_74 = () in
      let rec _unit_75 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_76 = () in
      let rec _unit_77 = () in
      let rec _unit_78 = () in
      let rec _unit_79 = () in
      let rec _unit_80 = () in
      let rec _unit_81 = () in
      let rec _unit_82 = () in
      let rec _unit_83 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_85 =
              let val_84 = result in
              Mpi.send (Marshal.to_string val_84 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_86 = loop () in ()
  | 4 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_87 = () in
      let rec _unit_88 = () in
      let rec _unit_89 = () in
      let rec _unit_90 = () in
      let rec _unit_91 = () in
      let rec _unit_92 = () in
      let rec _unit_93 = () in
      let rec _unit_94 = () in
      let rec _unit_95 = () in
      let rec _unit_96 = () in
      let rec _unit_97 = () in
      let rec _unit_98 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_99 = () in
      let rec _unit_100 = () in
      let rec _unit_101 = () in
      let rec _unit_102 = () in
      let rec _unit_103 = () in
      let rec _unit_104 = () in
      let rec _unit_105 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_107 =
              let val_106 = result in
              Mpi.send (Marshal.to_string val_106 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_108 = loop () in ()
  | 5 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_109 = () in
      let rec _unit_110 = () in
      let rec _unit_111 = () in
      let rec _unit_112 = () in
      let rec _unit_113 = () in
      let rec _unit_114 = () in
      let rec _unit_115 = () in
      let rec _unit_116 = () in
      let rec _unit_117 = () in
      let rec _unit_118 = () in
      let rec _unit_119 = () in
      let rec _unit_120 = () in
      let rec _unit_121 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_122 = () in
      let rec _unit_123 = () in
      let rec _unit_124 = () in
      let rec _unit_125 = () in
      let rec _unit_126 = () in
      let rec _unit_127 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_129 =
              let val_128 = result in
              Mpi.send (Marshal.to_string val_128 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_130 = loop () in ()
  | 6 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_131 = () in
      let rec _unit_132 = () in
      let rec _unit_133 = () in
      let rec _unit_134 = () in
      let rec _unit_135 = () in
      let rec _unit_136 = () in
      let rec _unit_137 = () in
      let rec _unit_138 = () in
      let rec _unit_139 = () in
      let rec _unit_140 = () in
      let rec _unit_141 = () in
      let rec _unit_142 = () in
      let rec _unit_143 = () in
      let rec _unit_144 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_145 = () in
      let rec _unit_146 = () in
      let rec _unit_147 = () in
      let rec _unit_148 = () in
      let rec _unit_149 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_151 =
              let val_150 = result in
              Mpi.send (Marshal.to_string val_150 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_152 = loop () in ()
  | 7 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_153 = () in
      let rec _unit_154 = () in
      let rec _unit_155 = () in
      let rec _unit_156 = () in
      let rec _unit_157 = () in
      let rec _unit_158 = () in
      let rec _unit_159 = () in
      let rec _unit_160 = () in
      let rec _unit_161 = () in
      let rec _unit_162 = () in
      let rec _unit_163 = () in
      let rec _unit_164 = () in
      let rec _unit_165 = () in
      let rec _unit_166 = () in
      let rec _unit_167 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_168 = () in
      let rec _unit_169 = () in
      let rec _unit_170 = () in
      let rec _unit_171 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_173 =
              let val_172 = result in
              Mpi.send (Marshal.to_string val_172 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_174 = loop () in ()
  | 8 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_175 = () in
      let rec _unit_176 = () in
      let rec _unit_177 = () in
      let rec _unit_178 = () in
      let rec _unit_179 = () in
      let rec _unit_180 = () in
      let rec _unit_181 = () in
      let rec _unit_182 = () in
      let rec _unit_183 = () in
      let rec _unit_184 = () in
      let rec _unit_185 = () in
      let rec _unit_186 = () in
      let rec _unit_187 = () in
      let rec _unit_188 = () in
      let rec _unit_189 = () in
      let rec _unit_190 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_191 = () in
      let rec _unit_192 = () in
      let rec _unit_193 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_195 =
              let val_194 = result in
              Mpi.send (Marshal.to_string val_194 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_196 = loop () in ()
  | 9 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_197 = () in
      let rec _unit_198 = () in
      let rec _unit_199 = () in
      let rec _unit_200 = () in
      let rec _unit_201 = () in
      let rec _unit_202 = () in
      let rec _unit_203 = () in
      let rec _unit_204 = () in
      let rec _unit_205 = () in
      let rec _unit_206 = () in
      let rec _unit_207 = () in
      let rec _unit_208 = () in
      let rec _unit_209 = () in
      let rec _unit_210 = () in
      let rec _unit_211 = () in
      let rec _unit_212 = () in
      let rec _unit_213 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_214 = () in
      let rec _unit_215 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_217 =
              let val_216 = result in
              Mpi.send (Marshal.to_string val_216 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_218 = loop () in ()
  | 10 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_219 = () in
      let rec _unit_220 = () in
      let rec _unit_221 = () in
      let rec _unit_222 = () in
      let rec _unit_223 = () in
      let rec _unit_224 = () in
      let rec _unit_225 = () in
      let rec _unit_226 = () in
      let rec _unit_227 = () in
      let rec _unit_228 = () in
      let rec _unit_229 = () in
      let rec _unit_230 = () in
      let rec _unit_231 = () in
      let rec _unit_232 = () in
      let rec _unit_233 = () in
      let rec _unit_234 = () in
      let rec _unit_235 = () in
      let rec _unit_236 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_237 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_239 =
              let val_238 = result in
              Mpi.send (Marshal.to_string val_238 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_240 = loop () in ()
  | 11 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_241 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_242 = () in
      let rec _unit_243 = () in
      let rec _unit_244 = () in
      let rec _unit_245 = () in
      let rec _unit_246 = () in
      let rec _unit_247 = () in
      let rec _unit_248 = () in
      let rec _unit_249 = () in
      let rec _unit_250 = () in
      let rec _unit_251 = () in
      let rec _unit_252 = () in
      let rec _unit_253 = () in
      let rec _unit_254 = () in
      let rec _unit_255 = () in
      let rec _unit_256 = () in
      let rec _unit_257 = () in
      let rec _unit_258 = () in
      let rec _unit_259 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_261 =
              let val_260 = result in
              Mpi.send (Marshal.to_string val_260 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_262 = loop () in ()
  | 12 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_263 = () in
      let rec _unit_264 = () in
      let rec _unit_265 = () in
      let rec _unit_266 = () in
      let rec _unit_267 = () in
      let rec _unit_268 = () in
      let rec _unit_269 = () in
      let rec _unit_270 = () in
      let rec _unit_271 = () in
      let rec _unit_272 = () in
      let rec _unit_273 = () in
      let rec _unit_274 = () in
      let rec _unit_275 = () in
      let rec _unit_276 = () in
      let rec _unit_277 = () in
      let rec _unit_278 = () in
      let rec _unit_279 = () in
      let rec _unit_280 = () in
      let rec _unit_281 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_283 =
              let val_282 = result in
              Mpi.send (Marshal.to_string val_282 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_284 = loop () in ()
  | 13 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_285 = () in
      let rec _unit_286 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_287 = () in
      let rec _unit_288 = () in
      let rec _unit_289 = () in
      let rec _unit_290 = () in
      let rec _unit_291 = () in
      let rec _unit_292 = () in
      let rec _unit_293 = () in
      let rec _unit_294 = () in
      let rec _unit_295 = () in
      let rec _unit_296 = () in
      let rec _unit_297 = () in
      let rec _unit_298 = () in
      let rec _unit_299 = () in
      let rec _unit_300 = () in
      let rec _unit_301 = () in
      let rec _unit_302 = () in
      let rec _unit_303 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_305 =
              let val_304 = result in
              Mpi.send (Marshal.to_string val_304 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_306 = loop () in ()
  | 14 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_307 = () in
      let rec _unit_308 = () in
      let rec _unit_309 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_310 = () in
      let rec _unit_311 = () in
      let rec _unit_312 = () in
      let rec _unit_313 = () in
      let rec _unit_314 = () in
      let rec _unit_315 = () in
      let rec _unit_316 = () in
      let rec _unit_317 = () in
      let rec _unit_318 = () in
      let rec _unit_319 = () in
      let rec _unit_320 = () in
      let rec _unit_321 = () in
      let rec _unit_322 = () in
      let rec _unit_323 = () in
      let rec _unit_324 = () in
      let rec _unit_325 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_327 =
              let val_326 = result in
              Mpi.send (Marshal.to_string val_326 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_328 = loop () in ()
  | 15 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_329 = () in
      let rec _unit_330 = () in
      let rec _unit_331 = () in
      let rec _unit_332 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_333 = () in
      let rec _unit_334 = () in
      let rec _unit_335 = () in
      let rec _unit_336 = () in
      let rec _unit_337 = () in
      let rec _unit_338 = () in
      let rec _unit_339 = () in
      let rec _unit_340 = () in
      let rec _unit_341 = () in
      let rec _unit_342 = () in
      let rec _unit_343 = () in
      let rec _unit_344 = () in
      let rec _unit_345 = () in
      let rec _unit_346 = () in
      let rec _unit_347 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_349 =
              let val_348 = result in
              Mpi.send (Marshal.to_string val_348 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_350 = loop () in ()
  | 16 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_351 = () in
      let rec _unit_352 = () in
      let rec _unit_353 = () in
      let rec _unit_354 = () in
      let rec _unit_355 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_356 = () in
      let rec _unit_357 = () in
      let rec _unit_358 = () in
      let rec _unit_359 = () in
      let rec _unit_360 = () in
      let rec _unit_361 = () in
      let rec _unit_362 = () in
      let rec _unit_363 = () in
      let rec _unit_364 = () in
      let rec _unit_365 = () in
      let rec _unit_366 = () in
      let rec _unit_367 = () in
      let rec _unit_368 = () in
      let rec _unit_369 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_371 =
              let val_370 = result in
              Mpi.send (Marshal.to_string val_370 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_372 = loop () in ()
  | 17 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_373 = () in
      let rec _unit_374 = () in
      let rec _unit_375 = () in
      let rec _unit_376 = () in
      let rec _unit_377 = () in
      let rec _unit_378 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_379 = () in
      let rec _unit_380 = () in
      let rec _unit_381 = () in
      let rec _unit_382 = () in
      let rec _unit_383 = () in
      let rec _unit_384 = () in
      let rec _unit_385 = () in
      let rec _unit_386 = () in
      let rec _unit_387 = () in
      let rec _unit_388 = () in
      let rec _unit_389 = () in
      let rec _unit_390 = () in
      let rec _unit_391 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_393 =
              let val_392 = result in
              Mpi.send (Marshal.to_string val_392 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_394 = loop () in ()
  | 18 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_395 = () in
      let rec _unit_396 = () in
      let rec _unit_397 = () in
      let rec _unit_398 = () in
      let rec _unit_399 = () in
      let rec _unit_400 = () in
      let rec _unit_401 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_402 = () in
      let rec _unit_403 = () in
      let rec _unit_404 = () in
      let rec _unit_405 = () in
      let rec _unit_406 = () in
      let rec _unit_407 = () in
      let rec _unit_408 = () in
      let rec _unit_409 = () in
      let rec _unit_410 = () in
      let rec _unit_411 = () in
      let rec _unit_412 = () in
      let rec _unit_413 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_415 =
              let val_414 = result in
              Mpi.send (Marshal.to_string val_414 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | "R" -> ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_416 = loop () in ()
  | 19 ->
      let rec gettimeofday arg = Unix.gettimeofday arg in
      let rec print_float arg = Stdlib.print_float arg in
      let rec sub_float arg = Stdlib.(-.) arg in
      let rec _unit_417 = () in
      let rec _unit_418 = () in
      let rec _unit_419 = () in
      let rec _unit_420 = () in
      let rec _unit_421 = () in
      let rec _unit_422 = () in
      let rec _unit_423 = () in
      let rec _unit_424 = () in
      let rec test_collatz inp =
        if inp = 1
        then 0
        else
          (let rec res1 = inp / 2 in
           let rec res2 = res1 * 2 in
           if res2 = inp
           then let rec part_res = test_collatz res1 in part_res + 1
           else
             (let rec res3 = (3 * inp) + 1 in
              let rec res4 = test_collatz res3 in 1 + res4)) in
      let rec _unit_425 = () in
      let rec _unit_426 = () in
      let rec _unit_427 = () in
      let rec _unit_428 = () in
      let rec _unit_429 = () in
      let rec _unit_430 = () in
      let rec _unit_431 = () in
      let rec _unit_432 = () in
      let rec _unit_433 = () in
      let rec _unit_434 = () in
      let rec _unit_435 = () in
      let rec loop iter =
        match Mpi.receive (loc_to_rank "P1") Mpi.any_tag Mpi.comm_world with
        | "R" -> ()
        | "L" ->
            let rec result = test_collatz 931386509544713451 in
            let rec _unit_437 =
              let val_436 = result in
              Mpi.send (Marshal.to_string val_436 []) (loc_to_rank "P1") 0
                Mpi.comm_world in
            loop ()
        | _ -> failwith "Runtime Error: Unmatched label" in
      let rec _unit_438 = loop () in ()
  | _ -> failwith "Runtime Error: Unknown rank"
