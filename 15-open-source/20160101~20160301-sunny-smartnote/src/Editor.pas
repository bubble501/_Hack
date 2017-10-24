////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   Editor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit Editor;

interface

uses Controls, Graphics,
     IntfEditor;

type
    {TssnEditor��һ�������࣬������ñ༭��Ӧ�ó�������������Ҫ�����б༭�����������---��Ϊ������
      Ȼ���ڴ����п��Է����ʵ�ֱ༭���������ʱ�滻
      ��ϸ����Delphi����ͻ�ơ��еĽ���˵��

    TssnEditor��ְ����Ϊ���еı༭��������г��󣬲�������ǵĹ�������
      ������Ҫʹ�õ�ʱ������Ҫ�����̳������ľ������
      }
    TssnEditor = class(IssnEditor)
    private
        {TssnEditor��Ϊ�༭��������༭���Ĳ����޷������ࣺ�ļ����ʣ���ȡ�ͱ��棩�����ֱ༭
          �������ֱ༭һ�������ı༭������������

          ��˵һ���ļ����ʣ��ļ�����Ҫ����TssnEditor�ڲ�������������Ӧ���ļ����ƣ����Զ���һ��private��m_FileNameΪ�ļ���
            �ڶ�ȡ�ļ�ʱ���øó�Ա��ֵ
            �ڡ����Ϊ��ʱ�ı�ó�Ա��ֵ
            �ڱ����ʱ��ֱ�ӽ��༭������е��ı����浽m_FileName��ָ���ļ�
          }
        m_FileName : String;
        
    protected
        {LoadFromFile��DoLoadFromFile
          LoadFromFile���㷨�ǽ�m_FileName����Ϊ��Ҫ�򿪵��ļ�����Ȼ����ļ���
          ����Ϊm_FileName����ֵ���������޷������ģ���Ҫ��TssnEditor��ʵ�֡�
          ��ʵ�ʴ��ļ��Ķ��������ı༭������йأ������ӳٵ��������������ʵ�֡�
          ���ǣ�virtual�Ϳ��԰���æ�ˡ�
          ���Զ���һ�����鷽��LoadFromFile�����������鷽��DoLoadFromFile��
            ����������ʵ�ֵ�DoLoadFromFileʵ�ʽ��ļ���ȡ���༭������У�
            ��LoadFromFile�������DoLoadFromFile������m_FileName��ֵ

        ���ģʽ����Ľ���
          ��������Ҳ�����ģʽ��һ�֣�����Ϊ��Template Method��������Ӧ�÷ǳ��ձ顣
          ��VCL�У�TObject�����Free()�������鷽��Destroy()֮��Ĺ�ϵ������ͬLoadFromFile()��DoLoadFromFile()�Ĺ�ϵһ��    
          }
        procedure DoLoadFromFile(FileName : String); virtual; abstract;

        {�÷����������������е�ʱ��ί��ģ�����
          ��ϸ��EditorEvent��WorkSpaceEvent��������Ԫ
          }
        procedure OnEditorSelectionChange(Sender : TObject);

        function GetText() : String; virtual; abstract;

    public
        procedure LoadFromFile(FileName : String);    //��ϸ��DoLoadFromFile���������ע��˵��

        {SaveToFile��Save��SaveAs
          �ļ����ʣ����˶�ȡ�ļ�����Ȼ���б����ļ���IssnEditor�ӿ��Ѿ���������������Save()��SaveAs()���ڱ��湦�ܣ���TssnEditor�п���ʵ������������
          SaveAs()���㷨����ʾһ�������Ϊ���ĶԻ������ָ������ȷ���ļ������򱣴��ı����ļ�
            ��LoadFromFile���ƣ�ֻ�����������ļ��Ķ�������ʵ�ʱ༭�������أ�������������һ��SaveToFile�ĳ����鷽������������ʵ�־���ı����ı��Ķ���
            �����������Ϊ���Ի��������ڹ����߼���������TssnEditor.SaveAs()��ʵ��
            ���Է��֣���ͬ����Template Methodģʽ��Ӧ��
          Save()���㷨�Ǽ��m_FileName�Ƿ�Ϊ���ַ������ǣ�δ�����ļ��������SaveAs()���������SaveToFile()�����ı�
          }
        procedure SaveToFile(FileName : String); virtual; abstract;
        function Save() : Boolean; override;
        function SaveAs() : Boolean; override;

        {GetText��GetWordCount
          TssnEditor���ļ����ʹ���������Ĵ��ļ��������ļ�����ط����ж��Ѿ�ʵ���ˣ�����Ӧ�������������ģ���DoLoadFromFile��SaveToFile��
          ʣ�µ����ֱ༭���ܻ����϶������ı༭������йأ����Ӧ��TssnEditor��������ʵ�֡�
          ���������е�ͳ���������ܣ������ڴ�׼�������б༭������һ�µ��㷨��������ͳ�ƣ���˾������ù�����TssnEditor��ʵ�֡�
          ͳ���������㷨�����Ȼ�ȡ�༭���е������ı����ַ�����ʽ����Ȼ�����ַ�����ͳ�ơ�
            Ҳ�����Ҳ�Ѿ��뵽��ȡ�ñ༭���е������ı��ַ�����Ҫ�ɾ�����ı༭�������TssnEditor�������ࣩ�ṩ��
            ��ͳ�Ʋ�������TssnEditor��ʵ�֣�������һ��Template Methodģʽ
            ��Ӧ�����鷽��GetText()�ͷ��鷽��GetWordCount()
          }
        function GetWordCount() : TssnWordCountRec; override;

        function GetFileName() : String; override;            //��ȡ�ļ���
    end;

implementation

uses GlobalObject;

{ TssnEditor }

function TssnEditor.GetFileName: String;
begin
    Result := m_FileName;
end;

{ TssnWordCountRec = record
        AnsiChar : Integer;
        MultiChar : Integer;
        NumChar : Integer;
        Other : Integer;
    end;
  }
function TssnEditor.GetWordCount: TssnWordCountRec;
var
    AllText : String;
    bHalf : Boolean;
    i : Integer;
    nAsc : Integer;
begin
    {��ȡ�༭���е������ı����ַ�����
      GetText()��TssnEditor�ĸ������������ʵ��
      }
    AllText := GetText();
    bHalf := false;

    {Ȼ�����ַ�����ͳ��
      String��ѯ�����ַ���˳���Ǵ� 1 ��ʼ�� Length(AllText)
      }
    for i := 1 to Length(AllText) do
    begin
        {ord�������ã�Char �����������ֵ��ת��������ȡÿ���ַ���ASCII����
          ��ϸ�μ���http://www.xumenger.com/delphi-ord-20160222/
          }
        nAsc := ord(AllText[i]);

        {�������˫�ֽ��ַ��ĵڶ����ַ�������Ը��ַ�}
        if bHalf then
        begin
            bHalf := false;
            nAsc := 0;
        end;

        if nAsc > 127 then                             //�����ǰ�Լ���ASCII���� >127������Ϊ��ʵ˫�ֽ��ַ�
        begin //chinese
            if not bHalf then
                Inc(Result.MultiChar);
            bHalf := not bHalf;
        end

        else if (nAsc >= 48) and (nAsc <= 57) then     //�����ַ���0-9
            Inc(Result.NumChar)

        else if (nAsc >= 65) and (nAsc <= 90) then     //Ӣ����ĸ�ַ���A-Z
            Inc(Result.AnsiChar)

        else if (nAsc >= 97) and (nAsc <= 122) then    //Ӣ����ĸ�ַ���a-z
            Inc(Result.AnsiChar)

        else if (nAsc <> 32) and (nAsc <> 0) and (nAsc <> 13) and (nAsc <> 10) then         //�����ַ�
            Inc(Result.Other);
    end;
end;

procedure TssnEditor.LoadFromFile(FileName: String);
begin
    m_FileName := FileName;

    {ʵ�ʶ�ȡ�ļ����༭���Ķ������ӳٵ�������ʵ��
      ���DoLoadFromFileΪvirtual; abstract;��
      ���FileName = '' ����ʾ�½�һ���༭������������κ��ļ�
      }
    if FileName <> '' then
        DoLoadFromFile(FileName);
end;

procedure TssnEditor.OnEditorSelectionChange(Sender: TObject);
begin
    g_EditorEvent.OnEditorSelectionChange(Sender);
end;

function TssnEditor.Save: Boolean;
begin
    {Save()���㷨�Ǽ��m_FileName�Ƿ�Ϊ���ַ�����
      �ǣ�δ�����ļ��������SaveAs()��
      �������SaveToFile()�����ı�
      }
    if m_FileName = '' then
    begin
        Result := SaveAs();
        Exit;
    end;

    SaveToFile(m_Filename);
    Result := true;
end;

function TssnEditor.SaveAs: Boolean;
var
    FileName : String;
begin
    Result := false;
    
    {ͨ�����������Ϊ���ĶԻ��򣬻�ȡ��Ҫ������ļ���
      }
    FileName := g_InterActive.ShowSaveDlg();
    if FileName = '' then
        Exit;
    SaveToFile(FileName);
    m_FileName := FileName;
    Result := true;
end;

end.
