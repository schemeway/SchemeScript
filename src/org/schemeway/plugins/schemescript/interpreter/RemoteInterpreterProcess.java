package org.schemeway.plugins.schemescript.interpreter;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.util.HashMap;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.dialogs.MessageDialog;

public class RemoteInterpreterProcess implements IProcess
{
    private static class StreamsProxy implements IStreamsProxy {
        private OutputStreamMonitor mMonitor;
        private PrintStream mReplInput;
        
        public StreamsProxy(InputStream is, OutputStream os) {
            mMonitor = new OutputStreamMonitor(is);
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
        } 
        catch (IOException e)  {
            MessageDialog.openError(null, getLabel(), "Unable to establish connection: " + e.getMessage());
            mSocket = null;
        }
    }
    
    public static RemoteInterpreterProcess getInstance() {
        if (mInstance == null) {
            mInstance = new RemoteInterpreterProcess();
        }
        return mInstance;
    }
    
    public String getLabel() {
        return "Remote interpreter (" + getHost() + ":" + getPort() + ")";
    }
    
    protected String getHost() {
        return "localhost"; // TODO
    }
    
    private int getPort() {
        return 5156;
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
        return mSocket != null && mSocket.isConnected();
    }

    public boolean isTerminated() {
        return mSocket == null || mSocket.isClosed();
    }

    public void terminate() throws DebugException {
        try {
            mSocket.close();
        } 
        catch (IOException e) {
        }
        mSocket = null;
        mInstance = null;
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
