////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   ssnPublic.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit ssnPublic;

interface

uses SysUtils, Forms;

const
    SSN_MAX_WORKSPACE   = 128;

    SSN_ENTER_CHAR      = #13 + #10;


    function GetExePath() : String;

implementation

var
    Path_ExePath : String = '';

{获取当前编辑器程序的exe路径，注意其中对 \ 的处理
  如果目录是F:\MyWork\Delphi\Sunny-SmartNote\Sunny-SmartNote-master\exe\snote.exe
  那么这个函数的返回值是F:\MyWork\Delphi\Sunny-SmartNote\Sunny-SmartNote-master\exe\
  }
function GetExePath() : String;
begin
    if Path_ExePath = '' then
    begin
        Path_ExePath := ExtractFilePath(Application.ExeName);
        if Path_ExePath[Length(Path_ExePath)] <> '\' then
            Path_ExePath := Path_ExePath + '\';
        Result := Path_ExePath
    end
    else
        Result := Path_ExePath;
end;

end.
