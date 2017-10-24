unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ADODB, DB, StdCtrls, ActiveX;

const
  Const_ConnStr = 'Provider=OraOLEDB.Oracle.1;Persist Security Info=False;User ID=%s;Password=%s;Data Source=%s';
  Const_ClickTime = '已经点击%d次';

type
  TForm1 = class(TForm)
    edtName: TEdit;
    edtPasswd: TEdit;
    edtDatabase: TEdit;
    lblname: TLabel;
    lblPasswd: TLabel;
    lblDatabase: TLabel;
    chkconntype: TCheckBox;
    btnStartTest: TButton;
    btnThreads: TButton;
    procedure btnStartTestClick(Sender: TObject);
    procedure btnThreadsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  //通过这个线程频繁和数据库建立连接
  TADOThread = class(TThread)
    procedure Execute(); override;
  end;

var
  Form1: TForm1;
  ClickTime: Integer = 0;
  threads: array[0..29] of TADOThread;
  isStop: Boolean = False;

implementation

{$R *.dfm}

procedure TADOThread.Execute();
var
  ConnectString: string;
  Query: TADOQuery;
  Connection: TADOConnection;
begin
  CoInitialize(nil);
  while not Terminated do
  begin
    ConnectString := Format(Const_ConnStr, [Form1.edtName.Text, Form1.edtPasswd.Text, Form1.edtDatabase.Text]);
    Query := TADOQuery.Create(nil);
    if Form1.chkconntype.Checked then
    begin
      //ADOQuery通过ADOConnection连接数据库
      Connection := TADOConnection.Create(nil);
      Connection.ConnectionString := ConnectString;
      Connection.LoginPrompt := False;
      Connection.Connected := True;

      Query.Connection := Connection;
    end
    else
    begin
      //ADOQuery通过赋值ConnectionString连接数据库
      Query.ConnectionString := ConnectString;
    end;

    Query.Close();
    Query.SQL.Clear();
    Query.SQL.Add('select * from tentrusts');
    Query.ExecSQL();

    if Form1.chkconntype.Checked then
    begin
      Connection.Close();
      Connection.Free();
    end;
    Query.Close();
    Query.Free();
  end;

  CoUninitialize();
end;

procedure TForm1.btnStartTestClick(Sender: TObject);
var
  ConnectString: string;
  Query: TADOQuery;
  Connection: TADOConnection;
begin
  Inc(ClickTime);
  btnStartTest.Caption := Format(Const_ClickTime, [ClickTime]);

  ConnectString := Format(Const_ConnStr, [edtName.Text, edtPasswd.Text, edtDatabase.Text]);
  Query := TADOQuery.Create(nil);
  if chkconntype.Checked then
  begin
    //ADOQuery通过ADOConnection连接数据库
    Connection := TADOConnection.Create(nil);
    Connection.ConnectionString := ConnectString;
    Connection.LoginPrompt := False;
    Connection.Connected := True;

    Query.Connection := Connection;
  end
  else
  begin
    //ADOQuery通过赋值ConnectionString连接数据库
    Query.ConnectionString := ConnectString;
  end;

  Query.Close();
  Query.SQL.Clear();
  Query.SQL.Add('select * from tentrusts');
  Query.ExecSQL();

  {
  //故意不释放连接，方便多次点击后查看数据库连接数情况
  if chkconntype.Checked then
  begin
    Connection.Close();
    Connection.Free();
  end;
  Query.Close();
  Query.Free();
  }
end;

procedure TForm1.btnThreadsClick(Sender: TObject);
var
  i: Integer;
begin
  if isStop then
  begin
    for i:=0 to 29 do
    begin
      threads[i].Terminate;
    end;
    btnThreads.Enabled := False;
  end
  else
  begin
    for i:=0 to 29 do
    begin
      threads[i] := TADOThread.Create(False);
    end;

    isStop := True;
  end;
end;

end.
