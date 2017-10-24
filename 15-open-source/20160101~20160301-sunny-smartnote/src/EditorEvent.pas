////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   EditorEvent.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-16
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit EditorEvent;

{�༭�������Ҫ�¼�ί�У�����Ҫί�е��¼�ֻ��һ�������༭��״̬�ĸ���
  �����κ�һ��״̬�����仯ʱ����ͨ��һ���¼�֪ͨ�����������Ӧ
  ��Ȼ��Ҳ���Խ�ÿ��״̬�ĸı��ֳɶ���¼�
����ϸ�Ľ��ͣ����ǿ���Delphi����ͻ�ơ����еĽ���
  }

interface

uses Classes;

type
    {�ڴ˴����༭�������״ֻ̬�������Ƿ������ơ����еȣ������߼���ͬ���ڱ༭����ѡ��״̬��
      ����ģ�����Ӧ������Զ������¼���ͬһ���¡���ȡ�༭����ѡ��״̬��ˢ�²˵�
      ��ˣ����Խ����ǹ鵽ͬһ���¼��У�OnEditorSelectionChange
      }
    {������ֻ����һ���¼������ֻ������public������һ���ɽ�������������ص�����ָ��
      ��һ���ɱ༭�����������֪ͨ��״̬����
      ��Ȼ�����ڲ�����һ��private�ĺ���ָ�����͵����ݳ�Ա���Ա��������ṩ�����Ļص�����ָ��
      }
    TssnEditorEvent = class
    private
        m_OnEditorChange : TNotifyEvent;    //�ص�����ָ��

    public
        procedure OnEditorSelectionChange(Sender : TObject);

        procedure SetOnEditorSelectionChange(Value : TNotifyEvent); //�����ģ��ͨ������SetOnEditorSelectionChange���������ûص�����ָ��
    end;

implementation

{ TssnEditorEvent }

{���༭���Ŀɱ༭״̬�����仯ʱ������OnEditorChange��֪ͨTssnEditorEvent��Ķ���
  Ȼ����TssnEditorEvent��ʵ��������ûص�����֪ͨ����ģ��������Ӧ
  }
procedure TssnEditorEvent.OnEditorSelectionChange(Sender : TObject);
begin
    if Assigned(m_OnEditorChange) then
        m_OnEditorChange(Sender);     //���ûص�����
end;

{�����ģ��ͨ������SetOnEditorSelectionChange���������ûص�����ָ��}
procedure TssnEditorEvent.SetOnEditorSelectionChange(Value : TNotifyEvent);
begin
    m_OnEditorChange := Value;
end;

end.
 