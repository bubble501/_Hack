////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SettingMgr.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-29
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit SettingMgr;

{ÿ��Ӧ����������й����ϵ�ѡ�������û����ã�����Щ���ÿ��ܻ��漰�������棬
  ����ڳ���������ض���Щѡ�����ý��й����Ƿǳ���Ҫ��
  ���Խ�Ĭ�ϵ�ѡ�����õĹ����߼�����������γ�һ���������࣬���������й�����ְ�����
    1.������ģ���ṩĳһ��ѡ���Ĭ������ֵ
    2.�����µ�ĳѡ��Ĭ������ֵ
    3.��Ӧ�ó�������ʱ�����ⲿ�����̻�ע���ȡ������ѡ���Ĭ������ֵ
    4.��Ӧ�ó����˳�ʱ�����������õ�ֵ���ⲿ�����̻�ע���
  }
{��������ΪTssnSettingMgr
  ���ȿ����㷨���ڳ�������ʱ��������ѡ���Ĭ��ֵ���ⲿ�����̣���ȡ���ڴ��С�
    ����ģ�������ȡĳ��ֵʱ�����ڴ��еĸ�ֵ���ظ�����
    ����ģ���������ĳ��ֵʱ�������ڴ��е�����
    ����Ҫ��ʱ�򣨳����˳�ʱ���߸��¹�ĳ��ֵʱ�����������õ�ֵ���浽�ⲿ�����̻�ע���
  Ȼ����ʵ�֣�ֻ��Ϊÿһ��ѡ���ṩһ���ڲ������ݳ�Ա������ֵ�����ṩ��Щֵ�ķ��ʽӿ�
    �ڸ��౻��ʼ��ʱ����������ʱ�������ⲿ�����̻�ע���ȡ��Ĭ������ֵ����ʼ����Щ���ݳ�Ա
    �ڸ��౻����ʱ�������˳�ʱ��������Щ���ݳ�Ա��ֵ���浽�ⲿ�����̻�ע���
    ���⣬�ͻ�����������ĳһ��ѡ���Ĭ��ֵ�ǣ�����Ҫ�����̱��浽�ⲿ�����̻�ע��������Ӱ�ȫ��
    TssnSettingMgrҲӦ�������������
  }
{��Sunny SmartNote�Ŀ���Դ��汾���֣�Ϊ�������ѡ������ģ��ֻ�������ù�������Ĭ������}

interface

uses Graphics, IniFiles;

type
    TssnSettingMgr = class
    private
        m_SaveWhenSet : Boolean;    //�Ƿ�������ѡ��Ĭ��ֵʱ���������浽�ⲿ�����̻�ע���

    protected
        {���õ����ƣ�������������ͬʵ�ֶ���ͬ
          ���磬����������õ��Ǵ����ļ���������ƿɱ���Ϊ�ļ���
          ����������õ�ע���������ƿɱ���������
          }
        m_SettingName : String;

        m_Font : TFont;   //����Ĭ����������ݳ�Ա

        {���������ѡ��ɼ���������ݳ�Ա}


        {���������������virtual�ģ���ζ����TssnSettingMgr�н�ά�����ṩһ��Ĭ�ϵ�ʵ�֡����Ӵ����ļ��д�ȡ
          ��Ȼ��TssnSettingMgr�����������override���������������ṩ��ͬ�Ĵ�ȡ��ʽ���籣�浽ϵͳע���}
        procedure LoadSettings(); virtual;  //���ⲿ�����̻�ע�����ȡ����Ĭ������ֵ
        procedure SaveSettings(); virtual;  //��������Ĭ������ֵ���ⲿ�����̻�ע���

    public
        procedure SetDefaultFont(Font : TFont);   //����Ĭ������ķ���
        function GetDefaultFont() : TFont;        //ȡ��Ĭ������ķ���

        {���������ѡ��ɼ������ķ���}

        constructor Create(SettingName : String; SaveWhenSet : Boolean);
        destructor Destroy(); override;
    end;

implementation

uses ssnPublic;

{ TssnSettingMgr }

constructor TssnSettingMgr.Create(SettingName: String; SaveWhenSet : Boolean);
begin
    m_SettingName := SettingName;
    m_SaveWhenSet := SaveWhenSet;

    m_Font := TFont.Create();;

    LoadSettings();    //��ȡ��������
end;

destructor TssnSettingMgr.Destroy;
begin
    SaveSettings();    //������������

    m_Font.Free();
end;

function TssnSettingMgr.GetDefaultFont: TFont;
begin
    Result := m_Font;
end;

procedure TssnSettingMgr.LoadSettings;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(GetExePath() + m_SettingName + '.ini');

    //�����ѡ��Ĭ��ֵ��ȡ
    m_Font.Name := IniFile.ReadString('Editor', 'FontName', 'Comic Sans MS');
    m_Font.Size := IniFile.ReadInteger('Editor', 'FontSize', 10);
    m_Font.Color := IniFile.ReadInteger('Editor', 'FontColor', clBlack);

    {���������ѡ��Ļ��������ڴ˼���}

    IniFile.Free();
end;

procedure TssnSettingMgr.SaveSettings;
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(GetExePath() + m_SettingName + '.ini');

    // �����ѡ��Ĭ��ֵ����
    IniFile.WriteString('Editor', 'FontName', m_Font.Name);
    IniFile.WriteInteger('Editor', 'FontSize', m_Font.Size);
    IniFile.WriteInteger('Editor', 'FontColor', m_Font.Color);

    {�����������ѡ��Ļ������ڴ˼���}

    IniFile.Free();
end;

procedure TssnSettingMgr.SetDefaultFont(Font: TFont);
begin
    m_Font.Assign(Font);

    if m_SaveWhenSet then   //�����Ҫ��������
        SaveSettings();
end;

end.
