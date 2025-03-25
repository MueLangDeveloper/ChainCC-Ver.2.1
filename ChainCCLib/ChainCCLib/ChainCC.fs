module ChainCCRunner
    open System
    open System.Linq
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open ChainCCSuporter

    type ChainCC(_code: string) =
        member val code: string array = _code.Replace("\r", "").Split('\n').Select(fun s -> s.Trim ' ').Where(fun s -> s <> "").ToArray() with get, set
        member val parseStr: string = "" with get, set
        member val success: bool = true with get, set
        member val ref: Dictionary<string, string> List = List<Dictionary<string, string>>() with get, set
        // as Queue
        member val stack: DynamicStack List = List<DynamicStack>() with get, set
        // as Queue
        member val vars: Dictionary<string, Dynamic> List = List<Dictionary<string, Dynamic>>() with get, set
        // as Queue
        member val result: string List = List<string>([|""|]) with get, set
        member val cmdLock: LockedCode List = List<LockedCode>() with get, set
        member val grammars: Dictionary<string, string List> = Dictionary<string, List<string>>() with get, set
        member val excutingGrammar: string List = List<string>() with get, set
        // as Queue
        member val operateIndex: int = -1 with get, set
        member val first: string = "" with get, set
        member val trims: char List = List<char>() with get, set
        member val private validCommands: string array = [|"match "; "if"; "loop"; "ref "; "result "; "delete "; "push "; "ope "; "let "; "set "; "result "; "lock "; "unlock "; "call "; "goto "; "gotob ";"gotonb"; "new_proc"; "delete_proc"; "pop"; "trim "; "pull"; "success "|]
        member val private operators: string array = [|"add"; "sub"; "mul"; "div"; "rem"; "pow"; "ceq"; "cneq"; "clt"; "cgt"; "ceqlt"; "ceqgt"; "and"; "or"; "rev"|]

        member _.StrEscape(_base: string): string =
            Regex.Replace(_base, @"\\.",
                fun m ->
                    match m.Value with
                    | "\\n" -> "\n"
                    | "\\t" -> "\\t"
                    | "\\\\" -> "\\"
                    | s -> s)

        member ccc.DivideGroupAndInvoke (_code: string List) =
            let inCode: string List = _code
            let mutable invokeStrs: string List = List<string>()
            let mutable appendFlg: bool = false
            let mutable index: int = 0

            while index < inCode.Count do
                match inCode[index] with
                | s when appendFlg && s.StartsWith ".after " ->
                    match s[(7 + s[7..].TakeWhile(fun c -> c = ' ').Count())..] with
                    | s when s[0] = '(' ->
                        let metaCode: string List = List<string>()
                        metaCode.Add s
                        index <- index + 1
                        while metaCode.Count = 0 || Regex.Replace(metaCode.Aggregate(fun x y -> x + y), @"""(\\.|[^""])*""", "")
                            .Where(fun c -> c = '(').Count() <> Regex.Replace(metaCode.Aggregate(fun x y -> x + y), @"""(\\.|[^""])*""", "")
                            .Where(fun c -> c = ')').Count() do
                            metaCode.Add inCode[index]
                            index <- index + 1
                        invokeStrs.Add(String.Join("\n", metaCode.Select(fun s -> s.Trim ' ')))
                        index <- index - 1
                    | s -> invokeStrs.Add s
                | s when ccc.validCommands.Any(fun cmd -> s.StartsWith cmd) ->
                    appendFlg <- true
                    invokeStrs.Add inCode[index]
                | "" -> ignore 0
                | _ -> ignore 0
                if index + 1 < inCode.Count && inCode[index + 1].StartsWith '.' = false && appendFlg then
                    appendFlg <- false
                    ccc.DividedGroupInvoke invokeStrs |> ignore
                    invokeStrs.Clear()
                if ccc.operateIndex >= 0 then
                    index <- ccc.operateIndex
                    ccc.operateIndex <- -1
                index <- index + 1
            ccc.result[0]

        member ccc.DividedGroupInvoke(codes: string List): string =
            let loopFlg: bool = codes[0].StartsWith "loop"
            let refFlg: bool = codes[0].StartsWith "ref"
            let mutable first: bool = true
            let mutable name: string = ""
            let mutable ifzero: bool = false
            let mutable canTrim: bool = true
            let mutable cres: string = ""
            
            let temporaryStr: string = ccc.parseStr

            if codes[0].StartsWith "lock" = false then
                if ccc.cmdLock[0].First <> "" then
                    ccc.cmdLock.Insert(0, LockedCode())
                    let res: string = ccc.DividedGroupInvoke (ccc.cmdLock[1].First.Split('\n').ToList())
                    if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + res
                    ccc.cmdLock.RemoveAt 0

                for _code in codes do
                    if ifzero = false then
                        if canTrim then ccc.parseStr <- String.Join("", ccc.parseStr.SkipWhile(fun c -> ccc.trims.Any(fun trim -> trim = c)))
                        let mutable code = _code
                        match code with
                        | c when c.StartsWith "match " ->
                            if ccc.success || first then
                                code <- code[6..].TrimStart ' '
                                match code with
                                | r when r.StartsWith '"' ->
                                    let mutable regex: Match = Regex.Match(code, @"^""((\\.|[^""])*)""")
                                    if regex.Success = false then raise(Exception())
                                    regex <- Regex.Match(ccc.parseStr, $"^({regex.Groups[1].Value})")
                                    if regex.Success = false then ccc.success <- false
                                    else
                                        ccc.parseStr <- ccc.parseStr[regex.Value.Length..]
                                        if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + regex.Value
                                        if first then ccc.success <- true
                                        first <- false
                                | r when r.StartsWith '<' ->
                                    let ref: string = Regex.Match(code, @"^<([A-Za-z0-9_]+)>").Groups[1].Value
                                    let regex: Match = Regex.Match(ccc.parseStr, $"^({ccc.ref[0][ref]})")
                                    if regex.Success = false then ccc.success <- false
                                    else
                                        ccc.parseStr <- ccc.parseStr[regex.Value.Length..]
                                        if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + regex.Value
                                        if first then ccc.success <- true
                                        first <- false
                                | r when r.StartsWith '$' ->
                                    let varName: string = Regex.Match(code, @"^\$([A-Za-z0-9_]+)").Groups[1].Value
                                    let regex: Match = Regex.Match(ccc.parseStr, $"^({ccc.vars[0][varName]})")
                                    if regex.Success = false then ccc.success <- false
                                    else
                                        ccc.parseStr <- ccc.parseStr[regex.Value.Length..]
                                        if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + regex.Value
                                        if first then ccc.success <- true
                                        first <- false
                                | r when r.StartsWith "value" ->
                                    let regex: Match = Regex.Match(ccc.parseStr, $"^({ccc.stack[0].Pop()})")
                                    if regex.Success = false then ccc.success <- false
                                    else
                                        ccc.parseStr <- ccc.parseStr[regex.Value.Length..]
                                        if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + regex.Value
                                        if first then ccc.success <- true
                                        first <- false
                                | _ -> raise(Exception())
                        | c when c.StartsWith "trim " ->
                            code <- code[5..].TrimStart ' '
                            match code with
                            | s when s.StartsWith "true" -> canTrim <- true
                            | s when s.StartsWith "false" -> canTrim <- false
                            | s when s.StartsWith "switch" -> canTrim <- canTrim = false
                            | _ -> raise(Exception())
                        | c when c.StartsWith "if " ->
                            code <- code[3..].TrimStart ' '
                            let reverse: bool = code.StartsWith "not "
                            if reverse then code <- code[4..]
                            code <- code.Trim ' '
                            match code with
                            | "match" -> if ccc.success = reverse then ifzero <- true
                            | "value" -> if ccc.stack[0].Pop() = reverse then ifzero <- true
                            | v when v.StartsWith '"' ->
                                let m: Match = Regex.Match(code, @"^""((\\.|[^""])+)""")
                                if m.Success = false then raise(Exception())
                                if Regex.IsMatch(ccc.parseStr, $"^({m.Groups[1].Value})") = reverse then ifzero <- true
                            | v when v.StartsWith '<' ->
                                let m: Match = Regex.Match(code, @"^<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                if Regex.IsMatch(ccc.parseStr, $"^({ccc.ref[0][m.Groups[1].Value]})") = reverse then ifzero <- true
                            | v when v.StartsWith '$' ->
                                let m: Match = Regex.Match(code, @"^\$([A-Za-z0-9_]+)")
                                if m.Success = false then raise(Exception())
                                if Regex.IsMatch(ccc.parseStr, $"^({ccc.vars[0][m.Groups[1].Value]})") = reverse then ifzero <- true
                            | v when v.StartsWith "?<" ->
                                let m: Match = Regex.Match(code, @"^\?<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                if ccc.ref[0].Keys.Any(fun r -> r = m.Groups[1].Value) = reverse then ifzero <- true
                            | _ -> raise(Exception())
                        | c when c.StartsWith "loop" -> ignore 0
                        | c when c.StartsWith "ref " ->
                            code <- code[4..].Trim ' '
                            name <- code
                            ccc.ref[0][name] <- ""
                        | c when c.StartsWith "push " ->
                            code <- code[5..].TrimStart ' '
                            match code with
                            | v when Regex.IsMatch(v, @"(-)?[0-9]+(\.[0-9]+)?(SB|B|S|US|I|UI|L|UL|F|D|M)") ->
                                let number: string = Regex.Match(v, @"(-)?[0-9]+(\.[0-9]+)?").Value
                                code <- code[number.Length..]
                                match code with
                                | ide when ide = "SB" -> ccc.stack[0].Push(number |> sbyte)
                                | ide when ide = "B" -> ccc.stack[0].Push(number |> byte)
                                | ide when ide = "S" -> ccc.stack[0].Push(number |> int16)
                                | ide when ide = "US" -> ccc.stack[0].Push(number |> uint16)
                                | ide when ide = "I" -> ccc.stack[0].Push(number |> int32)
                                | ide when ide = "UI" -> ccc.stack[0].Push(number |> uint32)
                                | ide when ide = "L" -> ccc.stack[0].Push(number |> int64)
                                | ide when ide = "UL" -> ccc.stack[0].Push(number |> uint64)
                                | ide when ide = "F" -> ccc.stack[0].Push(number |> single)
                                | ide when ide = "D" -> ccc.stack[0].Push(number |> double)
                                | ide when ide = "M" -> ccc.stack[0].Push(number |> decimal)
                                | _ -> raise(Exception("Could Not Found That Identifier."))
                            | v when Regex.IsMatch(v, @"""(\\.|[^""])*""|'(\\)?.'") ->
                                let mutable str: Dynamic = Dynamic((if v[0] = '"' then Regex.Match(v, @"""((\\.|[^""])*)""").Groups[1].Value else Regex.Match(v, @"'((\\)?.)'").Groups[1].Value) |> ccc.StrEscape)
                                if v[0] = '\'' then str <- Dynamic(str.Value |> Convert.ToChar)
                                ccc.stack[0].Push str.Value
                            | v when Regex.IsMatch(v, "^(true|false)") -> ccc.stack[0].Push(v.StartsWith "true")
                            | v when v.StartsWith "<" ->
                                let m: Match = Regex.Match(v, @"^<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                ccc.stack[0].Push(ccc.ref[0][m.Groups[1].Value])
                            | v when v.StartsWith "?<" ->
                                let m: Match = Regex.Match(v, @"^\?<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                ccc.stack[0].Push(ccc.ref[0].Any(fun kv -> kv.Key = m.Groups[1].Value))
                            | v when v.StartsWith '$' ->
                                code <- code[1..].Trim ' '
                                ccc.stack[0].Push((ccc.vars[0][code]).Value)
                            | v when v.StartsWith "stack" ->
                                ccc.stack[0].Push(ccc.stack[0].Count)
                            | _ -> raise(Exception("It Is Not Valid Identifier."))
                        | c when c.StartsWith "ope " ->
                            code <- code[4..].TrimStart ' '
                            let value2 = if code <> "rev" then ccc.stack[0].Pop() else 0
                            let value1 = ccc.stack[0].Pop()
                            ccc.stack[0].Push(ChainCCEval.Eval(ccc.operators.First(fun ope -> code.StartsWith ope), value1, value2))
                        | c when c.StartsWith "let " ->
                            code <- code[4..].Trim ' '
                            ccc.vars[0].Add(code, Dynamic(0))
                        | c when c.StartsWith "set " ->
                            code <- code[4..].TrimStart ' '
                            let varName: string = Regex.Match(code, @"[A-Za-z0-9_]+").Value
                            code <- code[varName.Length..].TrimStart ' '
                            match code with
                            | v when Regex.IsMatch(v, @"^(-)?[0-9]+(\.[0-9]+)?") ->
                                let number: string = Regex.Match(code, @"(-)?[0-9]+(\.[0-9]+)?").Value
                                code <- code[number.Length..]
                                match code with
                                | ide when ide = "SB" -> ccc.stack[0].Push(number |> sbyte)
                                | ide when ide = "B" -> ccc.stack[0].Push(number |> byte)
                                | ide when ide = "S" -> ccc.stack[0].Push(number |> int16)
                                | ide when ide = "US" -> ccc.stack[0].Push(number |> uint16)
                                | ide when ide = "I" -> ccc.stack[0].Push(number |> int32)
                                | ide when ide = "UI" -> ccc.stack[0].Push(number |> uint32)
                                | ide when ide = "L" -> ccc.stack[0].Push(number |> int64)
                                | ide when ide = "UL" -> ccc.stack[0].Push(number |> uint64)
                                | ide when ide = "F" -> ccc.stack[0].Push(number |> single)
                                | ide when ide = "D" -> ccc.stack[0].Push(number |> double)
                                | ide when ide = "M" -> ccc.stack[0].Push(number |> decimal)
                                | _ -> raise(Exception("Could Not Found That Identifier."))
                            | v when v.StartsWith '$' ->
                                code <- code[1..].TrimEnd ' '
                                ccc.stack[0].Push((ccc.vars[0][code]).Value)
                            | v when Regex.IsMatch(v, @"^""(\\.|[^""])*""|'(\\)?.'") ->
                                let mutable str: Dynamic = Dynamic((if v[0] = '"' then Regex.Match(v, @"""((\\.|[^""])*)""").Groups[1].Value else Regex.Match(v, @"'((\\)?.)'").Groups[1].Value) |> ccc.StrEscape)
                                if v[0] = '\'' then str <- Dynamic(str.Value |> Convert.ToChar)
                                ccc.stack[0].Push str.Value
                            | v when v.StartsWith "<" ->
                                let m: Match = Regex.Match(v, @"^<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                ccc.stack[0].Push(ccc.ref[0][m.Groups[1].Value])
                            | v when v.StartsWith "?<" ->
                                let m: Match = Regex.Match(v, @"^\?<([A-Za-z0-9_]+)>")
                                if m.Success = false then raise(Exception())
                                ccc.stack[0].Push(ccc.ref[0].Any(fun kv -> kv.Key = m.Groups[1].Value))
                            | v when Regex.IsMatch(v, "^(true|false)") -> ccc.stack[0].Push(v.StartsWith "true")
                            | v when v.StartsWith "value" -> ignore 0
                            | v when v.StartsWith "stack" ->
                                ccc.stack[0].Push(ccc.stack[0].Count)
                            | _ -> raise(Exception())
                            ccc.vars[0][varName] <- Dynamic(ccc.stack[0].Pop())
                        | c when c.StartsWith "result " ->
                            code <- code[7..].Trim ' '
                            match code with
                            | ide when ide.StartsWith "first" -> ccc.result[0] <- $"{ccc.stack[0].Pop()}" + ccc.result[0]
                            | ide when ide.StartsWith "last" -> ccc.result[0] <- ccc.result[0] + $"{ccc.stack[0].Pop()}"
                            | _ -> raise(Exception())
                        | c when c.StartsWith "delete " ->
                            code <- code[7..].Trim ' '
                            ccc.ref[0] <- Dictionary<string, string>(ccc.ref[0].Where(fun kv -> kv.Key <> code).ToArray())
                        | c when c.StartsWith "unlock " ->
                            code <- code[7..].Trim ' '
                            match code with
                            | "first" -> ccc.cmdLock[0].First <- ""
                            | "last" -> ccc.cmdLock[0].Last <- ""
                            | _ -> raise(Exception())
                        | c when c.StartsWith "call " ->
                            ccc.ref.Insert(0, Dictionary<string, string>())
                            ccc.stack.Insert(0, DynamicStack())
                            ccc.vars.Insert(0, Dictionary<string, Dynamic>())
                            code <- code[5..].Trim ' '
                            ccc.result.Insert(0, "")
                            ccc.excutingGrammar.Insert(0, code)
                            ccc.cmdLock.Insert(0, LockedCode())
                            cres <- ccc.DivideGroupAndInvoke ccc.grammars[code]
                            ccc.result.RemoveAt 0
                            ccc.ref.RemoveAt 0
                            ccc.stack.RemoveAt 0
                            ccc.vars.RemoveAt 0
                            ccc.excutingGrammar.RemoveAt 0
                            ccc.cmdLock.RemoveAt 0
                            if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + cres
                            cres <- ""
                        | c when c.StartsWith "goto " ->
                            code <- code[5..].Trim ' '
                            ccc.operateIndex <- ccc.grammars[ccc.excutingGrammar[0]].IndexOf($"#{code}")
                            ifzero <- true
                        | c when c.StartsWith "gotob " ->
                            code <- code[6..].Trim ' '
                            if ccc.stack[0].Pop() = true then
                                ccc.operateIndex <- ccc.grammars[ccc.excutingGrammar[0]].IndexOf($"#{code}")
                                ifzero <- true
                        | c when c.StartsWith "gotonb " ->
                            code <- code[6..].Trim ' '
                            if ccc.stack[0].Pop() = false then
                                ccc.operateIndex <- ccc.grammars[ccc.excutingGrammar[0]].IndexOf($"#{code}")
                                ifzero <- true
                        | c when c.StartsWith "new_proc" ->
                            ccc.cmdLock.Insert(0, LockedCode())
                            ccc.result.Insert(0, "")
                            ccc.ref.Insert(0, Dictionary<string, string>())
                            ccc.stack.Insert(0, DynamicStack())
                        | c when c.StartsWith "delete_proc" ->
                            ccc.cmdLock.RemoveAt 0
                            ccc.result.RemoveAt 0
                            ccc.ref.RemoveAt 0
                            ccc.stack.RemoveAt 0
                        | c when c.StartsWith "pop" ->
                            ccc.stack[0].Pop() |> ignore
                        | c when c.StartsWith "pull" ->
                            let value: string = $"{ccc.stack[0].Pop()}"
                            if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + value
                            ccc.result[0] <- ccc.result[0] + value
                        | c when c.StartsWith "success " ->
                            code <- code[8..].Trim ' '
                            ccc.success <- code.StartsWith "true"
                        | c when c.StartsWith '(' ->
                            let res: string = ccc.DivideGroupAndInvoke (code[1..].Split '\n' |> _.ToList())
                            if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + res
                        | c when Regex.IsMatch(c, @"^#[A-Za-z0-9_]+") -> ignore 0
                        | _ -> raise(Exception())
                        if canTrim then ccc.parseStr <- String.Join("", ccc.parseStr.SkipWhile(fun c1 -> ccc.trims.Any(fun c2 -> c1 = c2)))

                if ccc.cmdLock[0].Last <> "" then
                    ccc.cmdLock.Insert(0, LockedCode())
                    let res: string = ccc.DividedGroupInvoke (ccc.cmdLock[1].Last.Split('\n').ToList())
                    if refFlg then ccc.ref[0][name] <- ccc.ref[0][name] + res
                    ccc.cmdLock.RemoveAt 0
            else
                if Regex.IsMatch(codes[0], @"^lock\s+first$") then
                    ccc.cmdLock[0].First <- String.Join('\n', codes.ToArray()[1..])
                elif Regex.IsMatch(codes[0], @"^lock\s+last$") then
                    ccc.cmdLock[0].Last <- String.Join('\n', codes.ToArray()[1..])
                else raise(Exception())
            
            if ccc.success = false then ccc.parseStr <- temporaryStr
            if loopFlg && ccc.success then
                ccc.result[0] <- ccc.result[0] + ccc.DividedGroupInvoke codes
                ccc.success <- true
            if refFlg && ccc.success = false then ccc.ref[0] <- Dictionary<string, string>(ccc.ref[0].Where(fun kv -> kv.Key <> name).ToArray())
            
            ccc.result[0]

        member ccc.DivideLabel() =
            let codeList: string List = ccc.code.ToList()
            while codeList.Count <> 0 do
                match codeList[0] with
                | v when Regex.IsMatch(v, @"^trim\s+'\\?.'$") ->
                    let ch: char = Regex.Match(v, @"^trim\s+'(\\?.)'").Groups[1].Value |> ccc.StrEscape |> Convert.ToChar
                    ccc.trims.Add ch
                | v when Regex.IsMatch(v, @"^first\s+[A-Za-z0-9_]+$") ->
                    let gName: string = Regex.Match(v, @"^first\s+([A-Za-z0-9_]+)$").Groups[1].Value
                    ccc.first <- gName
                | v when Regex.IsMatch(v, @"^label\s+[A-Za-z0-9_]+$") ->
                    let gName: string = Regex.Match(v, @"^label\s+([A-Za-z0-9_]+)$").Groups[1].Value
                    codeList.RemoveAt 0
                    let mutable code: string = ""
                    while codeList[0] <> "end" do
                        code <- $"{code}{codeList[0]}\n"
                        codeList.RemoveAt 0
                    ccc.grammars.Add(gName, List<string>(code.Split '\n'))
                | v when v.StartsWith "end" -> ignore 0
                | _ -> raise(Exception())
                codeList.RemoveAt 0

        member ccc.Run(_parse: string) =
            ccc.parseStr <- _parse

            ccc.DivideLabel()

            ccc.ref.Insert(0, Dictionary<string, string>())
            ccc.stack.Insert(0, DynamicStack())
            ccc.vars.Insert(0, Dictionary<string, Dynamic>())
            ccc.excutingGrammar.Insert(0, ccc.first)
            ccc.cmdLock.Insert(0, LockedCode())

            if _parse.SkipWhile(fun c -> ccc.trims.Any(fun c2 -> c2 = c)).Count() <> 0 then
                ccc.DivideGroupAndInvoke ccc.grammars[ccc.first] |> ignore

            ccc.excutingGrammar.RemoveAt 0
            ccc.cmdLock.RemoveAt 0
            ccc.ref.RemoveAt 0
            ccc.stack.RemoveAt 0
            ccc.vars.RemoveAt 0

            ccc.result[0]