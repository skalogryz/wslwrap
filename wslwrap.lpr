program wslwrap;

{$mode objfpc}{$H+}

uses
  Windows,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, process;

// converts any entries of Hello#10World
// into
// Hello#13#10World
procedure WriteNixToWin(var dst: Text; const s: string);
var
  i : integer;
  j : integer;
begin
  j:=1;
  for i:=1 to length(s) do
    if (s[i]=#10) and (i<=length(s)) and (i>1) and (s[i-1]<>#13) then begin
      write(dst, copy(s, j, i-j));
      write(#13#10);
      j:=i+1;
    end;
  if j=1 then
    write(dst,s)
  else if j<>length(s)+1 then
    write(dst, copy(s, j,length(s)-j+1));
end;

// Replaces any entraces of X:\path\path as /mnt/x/path/path
// Does nothing with relative paths ./path/path
function WinPathToUnixPath(const s: string): string;
var
  i: integer;
  p : string;
begin
  i:=Pos(':\',s);
  if (i>0) and (s[i-1] in ['a'..'z','A'..'Z']) then begin
    Result := Copy(s, 1, i-2);
    p:=LowerCase(s[i-1])+Copy(s, i+1, length(s));
    p:=StringReplace(p, '\','/', [rfReplaceAll]);
    Result := Result+'/mnt/'+p;
  end else
    Result := s;
end;

// Returns the number of bytes available for the immediate (non-blocking) read from a handle
// A handle is assumed to be a standard input handle
function NumBytesAvailable(ahandle: THANDLE): integer;
const
  MaxDisk = 65536;
var
  lw, mx: DWORD;
  sz: Uint64;
  ofs: UInt64;
  cinp : array of TINPUTRECORD;
  rd: DWORD;
  i: integer;
begin
  case GetFileType(AHandle) of
    FILE_TYPE_PIPE:
      if not PeekNamedPipe(aHandle, nil, 0, nil, @Result, nil) then
        Result := 0;

    FILE_TYPE_CHAR:
    begin
      Result := 0;
      Exit;
      if not GetNumberOfConsoleInputEvents(aHandle, @Result) then begin
        Result := 0;
        Exit;
      end;
      if (Result = 0) then Exit;

      SetLength(cinp, Result);
      PeekConsoleInput(aHandle, @cinp[0], length(cinp), @rd);
      Result := 0;
      for i := 0 to rd-1 do
        if (cinp[i].EventType = KEY_EVENT) and cinp[i].Event.KeyEvent.bKeyDown then
          inc(Result, 1); // reading ANSI (1-byte) :(
      if Result = 0 then FlushConsoleInputBuffer(aHandle);
    end;

    FILE_TYPE_DISK:
    begin
      mx := 0;
      lw := GetFileSize(ahandle, @mx);
      sz := mx shl 32 or lw;

      lw := SetFilePointer(aHandle, 0, @mx, FILE_CURRENT);
      ofs := mx shl 32 or lw;
      sz := sz - ofs;

      if sz > MaxDisk then Result := MaxDisk
      else Result := Integer(sz);
    end;

  else
    Result := 0;
  end;
end;

procedure RunWSLWrap(const dstExecName: string = '');
var
  p : TProcess;
  i : integer;
  s : string;
  sz : integer;
  inp : THandleStream;
  outa: integer;
  erra: integer;
begin

  //AllocConsole;
  inp := THandleStream.Create(StdInputHandle);
  p := TProcess.Create(nil);
  try
    p.Executable := 'wsl';
    //p.Executable := 'C:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe';
    p.Parameters.add('-e');
    if dstExecName<>'' then
      p.Parameters.add(dstExecName);
    for i:=1 to ParamCount do
      p.Parameters.Add( WinPathToUnixPath(ParamStr(i)));
    //writeln(p.Parameters.Text);
    p.Options := [poUsePipes];

    p.Execute;

    outa := p.Output.NumBytesAvailable;
    erra := p.Stderr.NumBytesAvailable;
    while (p.Running) or (outa > 0) or (erra > 0) do
    begin
      if p.Running and (outa = 0) or (erra = 0) then
      begin
        sz := NumBytesAvailable(inp.Handle);
        if sz>0 then begin
          SetLength(s, sz);
          sz := inp.Read(s[1], sz);
          SetLength(s, sz);
          p.Input.Write(s, length(s));
        end;
      end;

      sz := outa;
      if sz>0 then begin
        SetLength(s, sz);
        sz := p.Output.Read(s[1], sz);
        if sz < length(s) then SetLength(s, sz);
        WriteNixToWin(StdOut, s);
      end;

      sz := erra;
      if sz>0 then begin
        SetLength(s, sz);
        sz := p.Stderr.Read(s[1], sz);
        if sz < length(s) then SetLength(s, sz);
        WriteNixToWin(StdErr, s);
      end;
      outa := p.Output.NumBytesAvailable;
      erra := p.Stderr.NumBytesAvailable;
    end;

  finally
    p.Free;
    inp.Free;
  end;
end;

type
  TWSLWrap = record
    cmd : string;
    linkres : Boolean;
  end;

function GetCommandFile(const wrpfile: string; out info: TWSLWrap): Boolean;
var
  st : TStringList;
begin
  info := Default(TWSLWrap);
  try
    st := TStringList.create;
    try
      st.LoadFromFile(wrpfile);
      info.cmd := st.Values['exe'];
      info.linkres := st.Values['linkres']<>'';
      Result := true;
      //if st.Count>0 then Result := st[0]
      //else Result := '';
    finally
      st.Free;
    end;
  except
    Result := false;
  end;
end;

// this is a hack to make cross-compile bin utils work
procedure UpdateLinkRes(const fn: string);
var
  st : TStringList;
  i  : integer;
begin
  try
    st := TStringList.Create;
    try
      st.LoadFromFile(fn);
      for i:=0 to st.Count-1 do
        st[i]:=WinPathToUnixPath(st[i]);
      st.SaveToFile(fn);
    finally
      st.Free;
    end;
  except
  end;
end;

var
  exename : string;
  wrpname : string;
  s   : string;
  cmd : string;
  i   : integer;
  info : TWSLWrap;
begin
  exename := ExtractfileName(ParamStr(0));
  s:=ChangeFileExt(exename,'');
  i := Pos('_',s);
  if (i>0) then begin
    cmd := Copy(s, i+1, length(s));
  end else
    cmd :='';

  wrpname := ChangeFileExt(ParamStr(0),'.wrp');
  if FileExists(wrpname) then begin
    GetCommandFile(wrpname, info);
    if info.cmd<>'' then cmd := info.cmd;
    if info.linkres then UpdateLinkRes( 'link.res') ;
  end;

  if (ParamCount<1) and (cmd = '') then Exit;

  //for i:=1 to ParamCount do writeln(i,': ', Paramstr(i));

  RunWSLWrap(cmd);
end.

