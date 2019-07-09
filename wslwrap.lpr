program wslwrap;

{$mode objfpc}{$H+}

uses
  Windows,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, process;

procedure WriteNixToWin(var dst: Text; const s: string);
var
  i : integer;
  j : integer;
begin
  j:=1;
  for i:=1 to length(s) do
    if (s[i]=#10) and (i<=length(s)) and (s[i+1]<>#13) then begin
      write(dst, copy(s, j, i-j+1));
      write(#13);
      j:=i+1;
    end;
  if j=1 then
    write(dst,s)
  else if j<>length(s)+1 then
    write(dst, copy(s, j,length(s)-j+1));
end;

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

procedure RunWSLWrap(const dstExecName: string = '');
var
  p : TProcess;
  i : integer;
  s : string;
  sz : integer;
begin
  //AllocConsole;
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

    while (p.Running) or (p.Output.NumBytesAvailable > 0) or (p.Stderr.NumBytesAvailable > 0) do
    begin
      if p.Running and (p.Output.NumBytesAvailable = 0) or (p.Stderr.NumBytesAvailable = 0) then
      begin
        s:=#0{#10#13};
        p.Input.Write(s, length(s));
        Sleep(1);
      end;
      //writeln(p.Running,' ',p.Output.NumBytesAvailable,' ',p.Stderr.NumBytesAvailable,' ',p.ExitCode,' ',p.ExitStatus);

      sz := p.Output.NumBytesAvailable;
      if sz>0 then begin
        SetLength(s, sz);
        sz := p.Output.Read(s[1], sz);
        if sz < length(s) then SetLength(s, sz);
        WriteNixToWin(StdOut, s);
      end;

      sz := p.Stderr.NumBytesAvailable;
      if sz>0 then begin
        SetLength(s, sz);
        sz := p.Stderr.Read(s[1], sz);
        if sz < length(s) then SetLength(s, sz);
        WriteNixToWin(StdErr, s);
      end;
   end;

  finally
    p.Free;
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
    if info.linkres then begin
      UpdateLinkRes( 'link.res') ;
    end;
  end;

  if (ParamCount<1) and (cmd = '') then Exit;

  //for i:=1 to ParamCount do writeln(i,': ', Paramstr(i));

  RunWSLWrap(cmd);
end.

