#include "sim.h"

SimElement::~SimElement() {
}

void SimElement::inSig(Signal* signal) {
    inputSignals.push_back(signal);
}

void SimElement::postCycle() {
    
}

void SimElement::propagateSignals() {
    for (auto& signal : inputSignals) {
        signal->propagate();
    }
}