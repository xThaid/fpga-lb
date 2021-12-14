#pragma once

#include <iostream>

class Signal {
public:

    virtual void propagate() {

    }

};

template <typename T>
class OutputSignal : public Signal {
public:

    OutputSignal(T val) : val(val) {
    }

    virtual void set(T val) {
        this->val = val;
    }

    virtual T get() {
        return val;
    }

private:
    T val;
};


template <typename T>
class ForwardingOutputSignal : public OutputSignal<T> {
public:

    ForwardingOutputSignal(T* source) : OutputSignal<T>(*source), source(source) {
    }

    void set(T val) {
    }

    T get() {
        return *source;
    }

private:
    T* source;
};

template <typename T>
class InputSignal : public Signal {
public:

    virtual void assign(OutputSignal<T>* source) {
        this->source = source;
    }

    virtual void propagate() {
        this->val = source->get();
    }

    T get() {
        return val;
    }

protected:
    T val;
    OutputSignal<T>* source;
};  

template <typename T>
class ForwardingInputSignal : public InputSignal<T> {
public:

    ForwardingInputSignal(T* sink) : sink(sink) {

    }

    virtual void propagate() {
        InputSignal<T>::propagate();
        *sink = InputSignal<T>::val;
    }

private:
    T* sink;
};  
