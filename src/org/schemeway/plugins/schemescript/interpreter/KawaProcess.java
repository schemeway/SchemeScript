/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import gnu.mapping.*;

import java.io.*;
import java.util.*;

import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;

public class KawaProcess implements IProcess
{
    private final static String PROMPT = "> "; 
    
    private static class KawaPortStreamMonitor extends OutputStream implements IStreamMonitor {

        private List mListeners = new LinkedList();

        public KawaPortStreamMonitor() {
            
        }
        
        public void addListener(IStreamListener listener)
        {
            if (!mListeners.contains(listener))
                mListeners.add(listener); 
        }

        public String getContents()
        {
            return "";
        }

        public void removeListener(IStreamListener listener)
        {
            if (mListeners.remove(listener));
        }
        
        // send the string to all listeners. 
        private void writeString(String text) {
            for (Iterator iter = mListeners.iterator(); iter.hasNext();) {
                IStreamListener listener = (IStreamListener) iter.next();
                listener.streamAppended(text, this);
            }
        }
        
        public void write(byte[] bytes) {
            char[] chars = new char[bytes.length];
            for (int i = 0; i < chars.length; i++) {
                chars[i] = (char) bytes[i];
            }
            writeString(new String(chars));
        }

        public void write(int b) {
            if ((char) b == '\r')
                return;
            char[] chars =
            {
                (char) b
            };
            writeString(new String(chars));
        }
        
    }
    
    private static class KawaStreamsProxy implements IStreamsProxy {
        
        private KawaPortStreamMonitor mErrorMonitor = new KawaPortStreamMonitor();
        private KawaPortStreamMonitor mOutputMonitor = new KawaPortStreamMonitor();
        
        public KawaStreamsProxy() {
            OutPort.setOutDefault(new OutPort(new OutputStreamWriter(mOutputMonitor), true, true));
            OutPort.setErrDefault(new OutPort(new OutputStreamWriter(mErrorMonitor), true, true));
        }

        public IStreamMonitor getErrorStreamMonitor()
        {
            return mErrorMonitor;
        }

        public IStreamMonitor getOutputStreamMonitor()
        {
            return mOutputMonitor;
        }

        public void write(String input) throws IOException {
            OutPort consumer = OutPort.outDefault();
            // TODO - emulate what is done is kawa.Shell
            try
            {
                gnu.expr.Interpreter.getInterpreter().eval(input, consumer);
            }
            catch (Throwable e)
            {
                // TODO - handle the exception by showing the Stack trace view...
            }
            consumer.write(PROMPT);
            consumer.flush();
        }
    }
    
    private        Map              mAttributes    = new HashMap();
    private        ILaunch          mLaunch;
    private static KawaProcess      mInstance      = null;
    private static KawaStreamsProxy mProxyInstance = new KawaStreamsProxy();
    

    private KawaProcess()
    {
        super();
        setAttribute(IProcess.ATTR_PROCESS_TYPE, "scheme");
    }

    public String getLabel()
    {
        return "Embedded Kawa";
    }

    public ILaunch getLaunch()
    {
        return mLaunch;
    }
    
    public void setLaunch(ILaunch launch) {
        mLaunch = launch;
    }

    public IStreamsProxy getStreamsProxy()
    {
        return mProxyInstance;
    }

    public void setAttribute(String key, String value)
    {
        mAttributes.put(key, value);
    }

    public String getAttribute(String key)
    {
        return (String)mAttributes.get(key);
    }

    public int getExitValue() throws DebugException
    {
        return 0;
    }

    public Object getAdapter(Class adapter)
    {
        return null;
    }

    public boolean canTerminate()
    {
        return false;
    }

    public boolean isTerminated()
    {
        return false;
    }

    public void terminate() throws DebugException
    {
    }
    
    static KawaProcess getInstance() {
        if (mInstance == null) {
            mInstance = new KawaProcess();
        }
        return mInstance;
    }

    static void eval(String text) {
        try {
            getInstance().getStreamsProxy().write(text);
        }
        catch (IOException exception) {
            // should not happen!
        }
    }
}
