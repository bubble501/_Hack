////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   InterActive.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-5
//  Comment     :   Default use Win32 API
//
//
////////////////////////////////////////////////////////////////////////////////

unit InterActive;

{Ӧ�ó����ٲ���Ҫ���û����������ǳ����л�ʹ�ô����ĸ��ֶԻ���
  Windows ϵͳAPI ҲΪ�û��ṩ�˱�׼�ıر��ĶԻ���
  һ������£� ϵͳ�ṩ�ı�׼�Ի����Ѿ����������û�����Ҫ��
  ����ʱ����Ӧ�ó���汾�����������ܻ᲻������ʹ�ñ�׼�Ի���
  ����������������仯�����ϣ�����жԻ������ܹ������������һ��仯��ʹ����������ΪЭ����ͳһ��
  ��ô�Ϳ�����Ҫ��д�Ի��򲿷���صĴ��롣
  }
{��ν�����������ı仯�����������Ӱ���أ��ǵģ��𰸻��ǡ�������}
{���ǣ� �ֳ����һ���Ի�����߼����ࡪ��TssnInterActive��
  ��������һ�׶Ի���ʹ�ù淶�� �����ṩ���ֶԻ��� ���ļ��� �ļ����Ϊ�� ���ҡ��滻�ȣ���
  ������ÿ�ַ��ĶԻ�������������ʵ�֡�
  ��Ȼ�������� TssnInterActive �и���ʹ�� Windows ��׼�Ի����Ĭ��ʵ�֣�
  ������Ӧ�ó���ʹ�� Windows ��׼�Ի���ʱ�����������£� �� �Ͳ���Ϊ��д�����������ֱ��ʹ�� TssnInterActive �ˡ�}
{Ϊÿ�ֶԻ����ṩһ�������Թ��ͻ�������á���
  MessageBox����Ϣ�򣩡�ShowFontDlg������ѡ��Ի��򣩡�ShowSaveDlg�������ļ��Ի��򣩡�
  ShowOpenDlg�����ļ��Ի��򣩡�ShowFindDlg�����ҶԻ��򣩡�ShowReplaceDlg�� �滻�Ի��򣩡�}

interface

uses Windows, Dialogs, Graphics, Classes;

type
    TssnOnFindEvent = procedure (FindText : String; Options : TFindOptions) of Object;
    TssnOnReplaceEvent = procedure (FindText, ReplaceText : String; Options : TFindOptions) of Object;

    TssnInterActive = class
    private
        {��������ʾĳЩ�Ի���ʱ�� ������Ҫָ���丸���ڵľ������ MessageBox����
          ����ڹ�������ʵ��ʱ�����ⲿ����һ�������ڵľ����������һ�����ݳ�Ա�С�
          ���⣬TssnInterActive ���м������ݳ�Ա�� ���Ƿֱ��Ƕ�Ӧ��ÿ�ֶԻ���� VCL ���е������ʵ����
          �� TSaveDialog�� TOpenDialog�� TFontDialog�� TFindDialog�� TreplaceDialog��
          ʹ����Щ�����ʵ�� TssnInterActive��
          }
        m_hWnd : HWND;           //���캯���������������
        m_SaveDlg : TSaveDialog; //�������ļ����Ի���
        m_FontDlg : TFontDialog; //������ѡ�񡱶Ի���
        m_OpenDlg : TOpenDialog; //�����ļ����Ի���

        {����Щ�Ի����У� TFindDialog �� TReplaceDialog �Ƚ����⣬ �����Ƿ�ģ̬�ģ� �����̺��¼��йء�
          ���磬 TFindDialog �ĶԻ�����ʾ�� ���̲߳��������Կ��Լ��������� ֱ���û������ˡ�������һ���� ��ť���� OnFind �¼����ߵ�����ȡ���� ��ť���رնԻ���
          ��ˣ���ҪΪ TFindDialog �� TReplaceDialog �ṩһ�׻ص����ơ�
          ������ TssnInterActive �ڲ�������������ָ�����͵����ݳ�Ա��
          �ڿͻ�������� ShowFindDlg �� ShowReplaceDlgʱ�� ����ص�����ָ�벢���������������ݳ�Ա�У�
          �ڶԻ�������� OnFind �� OnReplace���¼�������ʱ�����ÿͻ�����Ļص��������ɡ�

          ���ʲô��ģʽ���塢ʲô�Ƿ�ģʽ���壬�Լ����߳��������ִ��崦��ʱ���������
          }
        m_FindDlg : TFindDialog; //�����ҡ��Ի���
        m_ReplaceDlg : TReplaceDialog;  //���滻���Ի���
        m_OnFindCallBack : TssnOnFindEvent;       //�����ҡ��Ļص�����ָ��
        m_OnReplaceCallBack : TssnOnReplaceEvent; //���滻���Ļص�����ָ��
        procedure OnFind(Sender : TObject);
        procedure OnReplace(Sender : TObject);
        procedure OnReplaceFind(Sender : TObject);

    public
        constructor Create(MainWnd : HWND);
        destructor Destroy(); override;

        {�������ʾ�Ի��򷽷���ֻ�Ǽ򵥵ص��öԻ�������� TFontDialog ��ʵ������Execute()���������û�ѡ����ͨ������ֵ���ض��ѡ�}
        function MessageBox(Text, Caption : String; uType : Cardinal) : Integer; virtual;
        function ShowSaveDlg() : string; virtual;
        function ShowFontDlg() : TFont; virtual;
        function ShowOpenDlg() : TStrings; virtual;
        procedure ShowFindDlg(defFindText : String; pfOnFind : TssnOnFindEvent); virtual;
        procedure ShowReplaceDlg(defFindText : String; pfOnFind : TssnOnFindEvent; pfOnReplace : TssnOnReplaceEvent); virtual;
    end;

implementation


{ TssnInterActive }

{TssnInterActive ���ڹ��캯���д��������жԻ����ʵ���������ǽ��г�ʼ����
  �Ժ�ÿ���ͻ�������Ҫ��ʾ�Ի���ʱ���� TssnInterActive ��ʵ��������Щ�Ի������� Execute()������
  ��Ȼ�������㷨Ҳ���Ե���Щ�˷���Դ���Ի�������ڻ�û��ʹ��ʱ�ͱ����������ˣ���
  ���е������㷨����ʱ��������ʱ���� TssnInterActive ��ʵ�֣� ������Ӱ���������������ģ�顣
  �������ڴ˻��������ֱȽ��˷���Դ���㷨��ʵ�֡�
}
constructor TssnInterActive.Create(MainWnd: HWND);
begin
    m_OnFindCallBack := nil;
    m_hWnd := MainWnd;        //��������

    {��ʼ���Ի���ʵ��}
    m_SaveDlg := TSaveDialog.Create(nil);
    m_SaveDlg.Options := [ofOverwritePrompt, ofHideReadOnly];

    m_FontDlg := TFontDialog.Create(nil);

    m_OpenDlg := TOpenDialog.Create(nil);
    m_OpenDlg.Options := [ofAllowMultiSelect, ofFileMustExist];

    m_FindDlg := TFindDialog.Create(nil);
    m_FindDlg.OnFind := OnFind;     //�����¼�����

    m_ReplaceDlg := TReplaceDialog.Create(nil);
    m_ReplaceDlg.OnFind := OnReplaceFind; //�����¼�����
    m_ReplaceDlg.OnReplace := OnReplace;  //�����¼�����
end;

destructor TssnInterActive.Destroy;
begin
    m_ReplaceDlg.Free();
    m_ReplaceDlg := nil;

    m_FindDlg.Free();
    m_FindDlg := nil;

    m_OpenDlg.Free();
    m_OpenDlg := nil;

    m_FontDlg.Free();
    m_FontDlg := nil;

    m_SaveDlg.Free();
    m_SaveDlg := nil;

    m_hWnd := 0;
end;

function TssnInterActive.MessageBox(Text, Caption: String;
  uType: Cardinal): Integer;
begin
    Result := Windows.MessageBox(m_hWnd, PChar(Text), PChar(Caption), uType);
end;

{�ڴˣ���Ҫ����һ�²���/�滻�Ի���� OnFind �� OnReplace �¼�����
  TFindDialog�� TReplaceDialog �Ƿ�ģ̬�ĶԻ��򣬴����Ǻ� ���������̣߳�
  ֱ���û������ˡ����ҡ������滻��֮��İ�ť֮�󣬲Ż�������ǵ� OnFind�� OnReplace ֮����¼���
���Կ������ڹ��캯���������������Ĵ��룺m_FindDlg.OnFind := OnFind;
  ��ʱ�� ���Ѿ��� TFindDialog ����� OnFind �¼�ָ��ָ���� TssnInterActive �ڲ���һ�� OnFind ������
  Ҳ����˵�� TFindDialog ����� OnFind �¼����ӹ��������� ������ OnFind�����е����ⲿģ�鴫���ġ����ҡ��¼��Ļص�����ָ��
  }
procedure TssnInterActive.OnFind(Sender: TObject);
begin
    m_FindDlg.CloseDialog();
    if Assigned(m_OnFindCallBack) then        //���ÿͻ�����Ļص�����
        m_OnFindCallBack(m_FindDlg.FindText, m_FindDlg.Options);
end;

procedure TssnInterActive.OnReplace(Sender: TObject);
begin
    if Assigned(m_OnReplaceCallBack) then
        m_OnReplaceCallBack(m_ReplaceDlg.FindText, m_ReplaceDlg.ReplaceText, m_ReplaceDlg.Options);
end;

procedure TssnInterActive.OnReplaceFind(Sender: TObject);
begin
    if Assigned(m_OnFindCallBack) then
        m_OnFindCallBack(m_ReplaceDlg.FindText, m_ReplaceDlg.Options);
end;

procedure TssnInterActive.ShowFindDlg(defFindText : String; pfOnFind : TssnOnFindEvent);
begin
    m_OnFindCallBack := pfOnFind;
    m_FindDlg.FindText := defFindText;
    m_FindDlg.Execute();
end;

function TssnInterActive.ShowFontDlg: TFont;
begin
    Result := nil;
    if m_FontDlg.Execute() then
        Result := m_FontDlg.Font;
end;

function TssnInterActive.ShowOpenDlg: TStrings;
begin
    Result := nil;
    if m_OpenDlg.Execute() then
        Result := m_OpenDlg.Files;
end;

procedure TssnInterActive.ShowReplaceDlg(defFindText: String;
  pfOnFind: TssnOnFindEvent; pfOnReplace: TssnOnReplaceEvent);
begin
    m_OnReplaceCallBack := pfOnReplace;
    m_OnFindCallBack := pfOnFind;
    m_ReplaceDlg.Execute();
end;

function TssnInterActive.ShowSaveDlg: string;
begin
    Result := '';

    if m_SaveDlg.Execute() then
        Result := m_SaveDlg.FileName;
end;

end.
