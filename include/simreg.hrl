-ifndef(simreg).
-define(simreg, true).

-include_lib("mmayen/include/smpp.hrl").

-record(soap_response, {status, message, flag}).

-define(SMSC_HOST, "10.200.213.4").
-define(SMSC_PORT, 10000).
-define(SYSTEM_ID, "cisaccount").
-define(PASSWORD, "cisa").
-define(NUM_RX, 10).

-define(WSDL, "<?xml version='1.0' encoding='UTF-8'?>
<s0:definitions name='SendSmsServiceDefinitions' targetNamespace='http://mtnn/eai/simreg/ws/sreg' xmlns:s0='http://schemas.xmlsoap.org/wsdl/' xmlns:s1='http://mtnn/eai/simreg/ws/sreg' xmlns:s2='http://schemas.xmlsoap.org/wsdl/soap/'>
  <s0:types>
    <xs:schema attributeFormDefault='unqualified' elementFormDefault='qualified' targetNamespace='http://eai.mtn.ng/simreg/smssend' xmlns='http://eai.mtn.ng/simreg/smssend' xmlns:xs='http://www.w3.org/2001/XMLSchema'>
      <xs:element name='Header'>
        <xs:complexType>
          <xs:sequence>
            <xs:element minOccurs='0' name='System' type='xs:string'/>
            <xs:element minOccurs='0' name='TransactionID' type='xs:string'/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name='SendSmsRequest'>
        <xs:complexType>
          <xs:sequence>
            <xs:element minOccurs='1' name='msisdn' type='xs:string'>
              <xs:annotation>
                <xs:documentation>The phone number of the recipient</xs:documentation>
              </xs:annotation>
            </xs:element>
            <xs:element minOccurs='1' name='message' type='xs:string'>
              <xs:annotation>
                <xs:documentation>The message to be sent</xs:documentation>
              </xs:annotation>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name='SendSmsResponse'>
        <xs:complexType>
          <xs:sequence>
            <xs:element minOccurs='1' name='status' type='xs:string'>
              <xs:annotation>
                <xs:documentation>A numeric status code for the current operation</xs:documentation>
              </xs:annotation>
            </xs:element>
            <xs:element minOccurs='1' name='detail' type='xs:string'>
              <xs:annotation>
                <xs:documentation>
                    A textual description of the status of the current operation
                </xs:documentation>
              </xs:annotation>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:schema>
    <xs:schema attributeFormDefault='unqualified' elementFormDefault='qualified' targetNamespace='http://mtnn/eai/simreg/ws/sreg' xmlns:xs='http://www.w3.org/2001/XMLSchema'>
      <xs:import namespace='http://eai.mtn.ng/simreg/smssend'/>
      <xs:element name='Header'>
        <xs:complexType>
          <xs:sequence>
            <xs:element ref='get:Header' xmlns:get='http://eai.mtn.ng/simreg/smssend'/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name='Request'>
        <xs:complexType>
          <xs:sequence>
            <xs:element ref='get:SendSmsRequest' xmlns:get='http://eai.mtn.ng/simreg/smssend'/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name='Response'>
        <xs:complexType>
          <xs:sequence>
            <xs:element ref='get:SendSmsResponse' xmlns:get='http://eai.mtn.ng/simreg/smssend'/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:schema>
  </s0:types>
  <s0:message name='Request'>
    <s0:part element='s1:Header' name='request_header'/>
    <s0:part element='s1:Request' name='parameters'/>
  </s0:message>
  <s0:message name='Response'>
    <s0:part element='s1:Response' name='parameters'/>
  </s0:message>
  <s0:portType name='SendSms'>
    <s0:operation name='Request' parameterOrder='parameters'>
      <s0:input message='s1:Request'/>
      <s0:output message='s1:Response'/>
    </s0:operation>
  </s0:portType>
  <s0:binding name='SendSmsServiceSoapBinding' type='s1:SendSms'>
    <s2:binding style='document' transport='http://schemas.xmlsoap.org/soap/http'/>
    <s0:operation name='Request'>
      <s2:operation style='document'/>
      <s0:input>
        <s2:header message='s1:Request' part='request_header' use='literal'/>
        <s2:body parts='parameters' use='literal'/>
      </s0:input>
      <s0:output>
        <s2:body parts='parameters' use='literal'/>
      </s0:output>
    </s0:operation>
  </s0:binding>
  <s0:service name='SendSmsService'>
    <s0:port binding='s1:SendSmsServiceSoapBinding' name='SendSmsSoapPort'>
      <s2:address location='http://ebonyi:11581/SendSms'/>
    </s0:port>
  </s0:service>
</s0:definitions>").

-define(SENDSMS_RESPONSE_TEMPLATE, "<env:Envelope xmlns:env='http://schemas.xmlsoap.org/soap/envelope/'><env:Header/><env:Body><m:SendSmsResponse xmlns:m='http://mtnn/eai/simreg/ws/sreg'><sms:SendSmsResponse xmlns:sms='http://eai.mtn.ng/simreg/sms'><sms:status>~p</sms:status><sms:detail>~s</sms:detail></sms:SendSmsResponse></m:SendSmsResponse></env:Body></env:Envelope>").  

-endif.
