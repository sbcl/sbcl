import lldb

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f lldb_bt.sbcl_backtrace sbt')
    print("Custom backtrace command 'sbt' loaded.")

def sbcl_backtrace(debugger, command, result, internal_dict):
    thread = debugger.GetSelectedTarget().GetProcess().GetSelectedThread()
    debugger.GetCommandInterpreter().HandleCommand("settings set show-progress false", lldb.SBCommandReturnObject())
    if thread:
        for frame in thread:
            result.AppendMessage(format_frame(frame, internal_dict))

def format_frame(frame, internal_dict):
    frame_number = frame.GetFrameID()
    pc_address = frame.GetPCAddress()
    target = lldb.debugger.GetSelectedTarget()
    pc_load_addr = pc_address.GetLoadAddress(target)
    
    module_name = frame.module.GetFileSpec() if frame.module else "???"
    func = frame.GetFunction()
    
    if func:
        func_name = func.GetName()
        output = f"frame #{frame_number}: {pc_load_addr:#016x} {func_name} in `{module_name}`"
        return output

    symbol = frame.GetSymbol()
    if symbol.IsValid():
        func_name = symbol.GetName()
        return f"frame #{frame_number}: {pc_load_addr:#016x} {func_name} in `{module_name}`"

    custom_name = None
    options = lldb.SBExpressionOptions()
    options.SetUnwindOnError(False)
    fp = frame.GetFP()

    expression = f"""
        char buf[1024];
        FILE* stream = (FILE*)fmemopen(buf, sizeof(buf), "w");
        (void)print_backtrace_frame((char*){pc_load_addr}, (void*){fp}, -1, stream);
        (int)fclose(stream);
        (char*)strndup(buf, sizeof(buf));
    """
    result = target.EvaluateExpression(expression, options)

    if result.GetError().Fail():
         print(f"ERROR: Call to print_backtrace_frame failed: {result.GetError().GetCString()}")

    if result.IsValid() and result.GetValueAsSigned() != 0:
        string_address = result.GetValueAsUnsigned()
        error = lldb.SBError()
        memory_bytes = target.ReadMemory(lldb.SBAddress(string_address, target), 1024, error)
        
        if error.Success():
            null_pos = memory_bytes.find(b'\x00')
            if null_pos >= 0:
                raw_bytes = memory_bytes[:null_pos]
            else:
                raw_bytes = memory_bytes
            custom_name = raw_bytes.decode('utf-8', 'replace')
        free_expr = f"(void)free((void*){string_address})"
        target.EvaluateExpression(free_expr, options)

    if custom_name:
        return f"frame #{frame_number}: {pc_load_addr:#016x} {custom_name}"
    else:
        return f"frame #{frame_number}: {pc_load_addr:#016x} in `{module_name}`"
