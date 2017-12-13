# -*- coding: utf-8 -*-

from wsgiref.simple_server import make_server
from cgi import parse_qs, escape

from aliyunsdkdysmsapi.request.v20170525 import SendSmsRequest
from aliyunsdkdysmsapi.request.v20170525 import QuerySendDetailsRequest
from aliyunsdkcore.client import AcsClient
import uuid

from config import *


acs_client = AcsClient(ACCESS_KEY_ID, ACCESS_KEY_SECRET, REGION)


'''
Call aliyun SMS SDK API Send Phone Messages

@param business_id
@param phone_numbers, '15129311757,15129311757'
@param sign_name
@param template_code
@param template_param
'''
def aliyun_send_sms(business_id, phone_numbers, sign_name, template_code, template_param=None):
    smsRequest = SendSmsRequest.SendSmsRequest()
    smsRequest.set_TemplateCode(template_code)
    if template_param is not None:
        smsRequest.set_TemplateParam(template_param)
    smsRequest.set_OutId(business_id)
    smsRequest.set_SignName(sign_name);
    smsRequest.set_PhoneNumbers(phone_numbers)

    smsResponse = acs_client.do_action_with_exception(smsRequest)
    return smsResponse


'''
Simple WebHook Server Application Function
Listening Datadog's Monitor Webhook and handle

@param environ
@param start_response
'''
def application(environ, start_response):
    try:
        try:
            request_body_size = int(environ.get('CONTENT_LENGTH', 0))
        except (ValueError):
            request_body_size = 0

        # get post request content
        request_body = environ['wsgi.input'].read(request_body_size)
        d = parse_qs(request_body)

        # print post http content detail
        print(d)
        print(request_body)
        __business_id = uuid.uuid1()
        print(__business_id)
        params = "{\"name\": \"徐猛\", \"metric\": \"btc\", \"range\": \"5\"}"
        print(aliyun_send_sms(__business_id, PHONE_NUMBER, SIGNN_NAME, TEMPLATE_CODE, params))

        # make a response
        response_body = 'Datadog Webhook Response'
        status = '200 OK'
        response_headers = [('Content-Type', 'text/html'), ('Content-Length', str(len(response_body)))]
        start_response(status, response_headers)
        return [response_body]
    except Exception,e:
        print(e.message)


if '__main__' == __name__:
    httpd = make_server('0.0.0.0', 12000, application)
    print('HTTP Server on port 12000')
    httpd.serve_forever()
