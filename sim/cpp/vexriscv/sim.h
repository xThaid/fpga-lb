#pragma once

#include "signal.h"

#include <vector>

class SimElement {
public:
    virtual ~SimElement();

    void inSig(Signal* signal);

    virtual void reset() = 0;
    virtual void cycle() = 0;

    virtual void postCycle();

    void propagateSignals();

private:
    std::vector<Signal*> inputSignals;
};