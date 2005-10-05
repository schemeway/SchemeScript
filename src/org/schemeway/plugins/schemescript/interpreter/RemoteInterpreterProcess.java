/*
 * Copyright (c) 2004-2005 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.net.SocketException;
import java.util.HashMap;

import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.dialogs.MessageDialog;
import org.schemeway.plugins.schemescript.preferences.RemoteInterpreterPreferences;

public class RemoteInterpreterProcess implements IInterpreterProcess
{
    private static class StreamsProxy implements IStreamsProxy {
        private OutputStreamMonitor mMonitor;
        private PrintStream mReplInput;
        
        public StreamsProxy(InputStream is, OutputStream os) {
            SocketExceptionHandler listener = new SocketExceptionHandler() {
                public void exceptionOccurred(SocketException exception) {
                    try {
                        mInstance.terminate();
                    } catch (DebugException e) {
                    }
                }
            };
            mMonitor = new OutputStreamMonitor(is, listener);
            mMonitor.startMonitoring();
            mReplInput = new PrintStream(os);
        }
        
        public IStreamMonitor getErrorStreamMonitor()
        {
            return null;
        }

        public IStreamMonitor getOutputStreamMonitor()
        {
            return mMonitor;
        }

        public void write(String input) throws IOException
        {
            mReplInput.print(input);
            if (mReplInput.checkError()) {
                try {
                    mInstance.terminate();
                }
                catch (DebugException exception) {
                }
            }
        }
    }
    
    private static RemoteInterpreterProcess mInstance = null;
    
    private Socket mSocket;
    private IStreamsProxy mProxy;
    private ILaunch mLaunch;
    private HashMap mAttributes = new HashMap();
    
    private RemoteInterpreterProcess() {
        try {
            mSocket = new Socket(getHost(), getPort());
            mProxy = new StreamsProxy(mSocket.getInputStream(), mSocket.getOutputStream());
            fireCreationEvent();
        } 
        catch (IOException e)  {
            MessageDialog.openError(null, getLabel(), "Unable to establish connection: " + e.getMessage());
            mSocket = null;
        }
    }
    
    public static RemoteInterpreterProcess getInstance() {
        if (mInstance == null) {
            mInstance = new RemoteInterpreterProcess();
            if (!mInstance.isRunning())
                mInstance = null;
        }
        return mInstance;
    }
    
    public String getLabel() {
        return "REPL on " + getHost() + ":" + getPort();
    }
    
    protected String getHost() {
        return RemoteInterpreterPreferences.getInterpreterHost();
    }
    
    private int getPort() {
        return RemoteInterpreterPreferences.getInterpreterPort();
    }

    public ILaunch getLaunch() {
        return mLaunch;
    }
    
    public void setLaunch(ILaunch launch) {
        mLaunch = launch;
    }

    public IStreamsProxy getStreamsProxy()
    {
        return mProxy;
    }

    public void setAttribute(String key, String value) {
        mAttributes.put(key, value);
    }

    public String getAttribute(String key) {
        return (String) mAttributes.get(key);
    }

    public int getExitValue() throws DebugException {
        return 0;
    }

    public Object getAdapter(Class adapter) {
        return null;
    }

    public boolean isRunning() {
        return mInstance != null && canTerminate();
    }
    
    public boolean canTerminate() {
        return (mSocket != null) && (mSocket.isConnected());
    }

    public boolean isTerminated() {
        return (mSocket == null) || (mSocket.isClosed());
    }

    public void terminate() throws DebugException {
        try {
            if (mSocket != null)
                mSocket.close();
        } 
        catch (IOException e) {
        }
        finally {
            mSocket = null;
            terminated();
        }
    }

    private void terminated()
    {
        fireEvent(new DebugEvent(this, DebugEvent.TERMINATE));
        mInstance = null;
    }

    private void fireCreationEvent() {
        fireEvent(new DebugEvent(this, DebugEvent.CREATE));
    }
    
    private void fireEvent(DebugEvent event)
    {
        DebugPlugin.getDefault().fireDebugEventSet(new DebugEvent[] { event });
    }
    
    public void sendToInterpreter(String text) {
        try {
            mProxy.write(text);
        } 
        catch (IOException e) {
            // TODO Auto-generated catch block
        }
    }
}
