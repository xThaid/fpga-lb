#include "memctrl.h"

IBusCtrl::IBusCtrl() :
    avalonDataValid(0),
    avalonData(0),
    avalonWaitRequestn(1),
    avalonResponse(0) {

    inSig(&avalonAddress);
    inSig(&avalonRead);
}

void IBusCtrl::reset() {
}

void IBusCtrl::cycle() {
    if (avalonRead.get() && avalonWaitRequestn.get()) {
        IBusAvalonRsp rsp;
        rsp.data = read(avalonAddress.get());
        rsp.error = false;
        respQ.push(rsp);
    }

    if (!respQ.empty()) {
        IBusAvalonRsp rsp = respQ.front(); respQ.pop();
        avalonData.set(rsp.data);
        avalonDataValid.set(1);
        avalonResponse.set(rsp.error ? 3 : 0);
    } else {
        avalonDataValid.set(0);
    }
}

void IBusCtrl::postCycle() {

}

DBusCtrl::DBusCtrl() :
    avalonDataValid(0),
    avalonData(0),
    avalonWaitRequestn(1),
    avalonResponse(0) {

    inSig(&avalonAddress);
    inSig(&avalonRead);
    inSig(&avalonByteEnable);
    inSig(&avalonWrite);
    inSig(&avalonWriteData);
}

void DBusCtrl::reset() {
}

void DBusCtrl::cycle() {
    if (avalonWrite.get() && avalonWaitRequestn.get()) {
        write(avalonAddress.get(), avalonByteEnable.get(), avalonWriteData.get());
    }

    if (avalonRead.get() && avalonWaitRequestn.get()) {
        DBusAvalonRsp rsp;
        rsp.data = read(avalonAddress.get());
        rsp.error = false;
        respQ.push(rsp);
    }

    if (!respQ.empty()) {
        DBusAvalonRsp rsp = respQ.front(); respQ.pop();
        avalonData.set(rsp.data);
        avalonDataValid.set(1);
        avalonResponse.set(rsp.error ? 3 : 0);
    } else {
        avalonDataValid.set(0);
    }
}

void DBusCtrl::postCycle() {
    
}
