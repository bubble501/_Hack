program snote;

uses
  {2016-02-21��
    �����Ŀ�����ڿ���Delphi����ͻ�ơ��Ȿ���ʱ���Ȿ������һ����ר�������ʵ�������н���
    2015-10-17���ҵ�ʱ����ר�Ž��Ȿ�鿴��һ�飬���ǵ�ʱ��Ϊ�Լ���Delphi����ⲻ���롢�������鲻�����������кིܶ�ⶼ����⣬���������Ŀʵ��Ҳû�п�
    �����ھ������Լ�ƽʱ���˼��ε���Ŀ�������顢�Լ�ƽʱ����ѧϰ������Delphi��֪ʶ����������ת������ܵĴ��룬��Delphi����һЩ��⣬�������ֿ������Ŀ
    �������ڴ��������ע�͵���ʽ�������Լ���ȥ�������г��򣬳����Լ���һЩ���Ի����޸ġ����һЩ�µĹ��ܣ��������Լ������
    �顶Delphi����ͻ�ơ��Ȿ��ĵ�7�����й������С��Ŀ�ļܹ��Ľ��⣬��������������ơ���ķ�װ����
      �Ƚ����е�֪ʶ���濴һ�飬�����С��Ŀ��ϵͳ�ܹ���һ������İ��գ�Ȼ����ȥ����鿴����ϸ��
      ��ķ�װ���ɡ�Delphi����Ϣ���ơ�Delphi�Ŀؼ���������֪ʶ���ǿ���ͨ�������Ŀѧϰ���ģ�
    ���Զ��������Ŀ��һ������һ��Ҫ����Delphi����ͻ�ơ��Ȿ�����������濴��������ǽ����ȥ�����о������Ŀ�Ĵ���ϸ��
  }
  {ÿ����Ԫǰ�����ű�ʾ�о������ĿԴ�������ο��ĵ�Ԫ��˳��
  2016-02-22��
    ������WorkSpaceMgr��Ԫ�Ĵ���ʵ�֣�����GlobalObject��Ԫ����Ҫ���������ٵ����б�����û�н���ע�ͣ��������
  2016-02-33 23:30��
    ��ϡ�Delphi����ͻ�ơ������С��Ŀ��Դ�뿴��һ�飬��������ϵͳ����һ������
    ���ǻ���û�ж�ÿһ��ϸ�ڶ���⣬���Ի���Ҫ�Լ���ȥ���ϸ�����ϸ�ڵĿ������ҳ����޸ĳ���-->����-->����-->����-->���޸�-->����
    }
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},   //16.�༭�������壬��Ҫ�Ƿ��ÿؼ�����ƣ�����Ĺ����㷨�߼�������������Ԫ�����װ���ڴ˵�Ԫ���ÿؼ����������������߼�
  Editor in 'Editor.pas',                    //2.������TssnEditor�࣬���༭�������
  ssnPublic in 'ssnPublic.pas',              //6.������һЩ������GetExePath����
  WorkSpaceMgr in 'WorkSpaceMgr.pas',        //8.������������
  WorkSpace in 'WorkSpace.pas',              //5.������TssnWorkSpace�࣬��������������ΪTssnEditor�Ĵ���
  TabWorkSpaceMgr in 'TabWorkSpaceMgr.pas',  //9.TssnWorkSpace��TssnWorkSpaceMgr���ǳ����࣬�����Ԫ�ֱ�Ϊ��ʵ���˾��������
  GlobalObject in 'GlobalObject.pas',        //8--16.������һЩȫ�ֱ��������󡭡����ĳ�ʼ���������ͷŷ���
  EditorCtor in 'EditorCtor.pas',            //10.�༭������ġ���������
  MemoEditor in 'MemoEditor.pas',            //3.������TssnMemoEditor�࣬�̳���TssnEditor��
  WorkSpaceMgrCtor in 'WorkSpaceMgrCtor.pas',//11.�������������ġ���������
  MultiLan in 'MultiLan.pas',                //17.�õ�Ԫ��Ϊ��ʵ�ֶ����԰汾��׼����
  InterActive in 'InterActive.pas',          //15.��װ�������û��������࣬��Ҫ�Ǹ��ֵ�����
  IntfEditor in 'IntfEditor.pas',            //1.������IssnEditor�����࣬TssnEditor��TssnWorkSpace���̳���������֤TssnWorkSpace������ȷ����TssnEditor
  SettingMgr in 'SettingMgr.pas',            //14.Ĭ�����ù���
  EditorEvent in 'EditorEvent.pas',          //12.�༭��������¼�ί��
  WorkSpaceEvent in 'WorkSpaceEvent.pas',    //13.���������¼�ί��
  RichEditor in 'RichEditor.pas';            //4.������TssnRichEditor�࣬�̳���TssnEditor��ʵ��һ���򵥵ĸ��ı��༭�����

{$R *.RES}

begin
    Application.Initialize;
    try
        InitObjects();    //����ʼʱ���ȴ���������Ҫ�Ķ��󣬰�������������ȵĴ��붼����������
        Application.Title := 'Sunny SmartNote 5';
        Application.Run;  //ͨ���������ԣ�����Delphi��Ŀ����ʵ�����𿪾ͻᵥ��ֹͣ��������
                          //Application.Run()���ǽ�������Ϣѭ������Ӧ������Ϣ�������û�����������淢������Ϣ����
    finally
        UnInitObjects();  //�������ʱ���ͷ�ͨ��InitObjects�����Ķ���
    end;
end.
