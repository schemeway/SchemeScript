/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import gnu.expr.*;
import gnu.kawa.functions.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.text.*;

import java.io.*;
import java.util.*;

import kawa.standard.*;

import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;
import org.schemeway.plugins.schemescript.views.*;

public class KawaProcess implements IProcess {
    private final static String PROMPT = "> ";

    private static class KawaPortStreamMonitor extends OutputStream implements IStreamMonitor {

        private List mListeners = new LinkedList();
        private boolean mIsStdout = false;

        public KawaPortStreamMonitor(boolean isStdout) {
            mIsStdout = isStdout;
        }

        public void addListener(IStreamListener listener) {
            if (!mListeners.contains(listener))
                mListeners.add(listener);
        }

        public String getContents() {
            return (mIsStdout ? PROMPT : "");
        }

        public void removeListener(IStreamListener listener) {
            if (mListeners.remove(listener))
                ;
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

        private KawaPortStreamMonitor mErrorMonitor = new KawaPortStreamMonitor(false);
        private KawaPortStreamMonitor mOutputMonitor = new KawaPortStreamMonitor(true);

        public KawaStreamsProxy() {
            OutPort.setOutDefault(new OutPort(new OutputStreamWriter(mOutputMonitor), false, true));
            OutPort.outDefault().objectFormat = DisplayFormat.getSchemeFormat(true);
            OutPort.setErrDefault(new OutPort(new OutputStreamWriter(mErrorMonitor), false, true));
        }

        public IStreamMonitor getErrorStreamMonitor() {
            return mErrorMonitor;
        }

        public IStreamMonitor getOutputStreamMonitor() {
            return mOutputMonitor;
        }

        public void write(String input) throws IOException {
            OutPort out = OutPort.outDefault();
            OutPort err = OutPort.errDefault();
            out.setColumnNumber(0);
            KawaProcess.eval(Scheme.getInstance(), new CharArrayInPort(input), out, err);
            if (out.getColumnNumber() != 0) {
                out.freshLine();
            }
            out.write(PROMPT);
            out.flush();
        }
    }

    private Map mAttributes = new HashMap();
    private ILaunch mLaunch;
    private static KawaProcess mInstance = null;
    private static KawaStreamsProxy mProxyInstance = new KawaStreamsProxy();

    private KawaProcess() {
        super();
    }

    public String getLabel() {
        return "Embedded Kawa";
    }

    public ILaunch getLaunch() {
        return mLaunch;
    }

    public void setLaunch(ILaunch launch) {
        mLaunch = launch;
    }

    public IStreamsProxy getStreamsProxy() {
        return mProxyInstance;
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

    public boolean canTerminate() {
        return false;
    }

    public boolean isTerminated() {
        return false;
    }

    public void terminate() throws DebugException {
    }

    static KawaProcess getInstance() {
        if (mInstance == null) {
            mInstance = new KawaProcess();
        }
        return mInstance;
    }

    static void sendToInterpreter(String text) {
        try {
            getInstance().getStreamsProxy().write(text);
        }
        catch (IOException exception) {
            // should not happen!
        }
    }

    static private int mCounter = 0;

    // Code adapter from Kawa.Shell
    static void eval(Scheme interp, InPort inp, OutPort out, OutPort perr) {
        Environment env = Scheme.getInstance().getEnvironment();
        SourceMessages messages = new SourceMessages();
        Lexer lexer = interp.getLexer(inp, messages);
        lexer.setInteractive(false);
        CallContext ctx = CallContext.getInstance();
        Consumer saveConsumer = null;
        if (out != null) {
            saveConsumer = ctx.consumer;
            ctx.consumer = out;
        }
        try {
            int opts = gnu.expr.Interpreter.PARSE_IMMEDIATE;
            try {
                Compilation comp = interp.parse(lexer, opts);
                boolean sawError = messages.checkErrors(perr, 20);
                if (comp == null) // ??? end-of-file
                    return; // break;
                if (sawError)
                    return; // continue;
                comp.getModule().setName("atInteractiveLevel$" + (++mCounter));

                ModuleExp.evalModule(env, ctx, comp);
                if (messages.checkErrors(perr, 20))
                    return;
                ctx.runUntilDone();
            }
            catch (WrongArguments e) {
                showError(e, perr);
                return;
            }
            catch (java.lang.ClassCastException e) {
                showError(e, perr);
                return;
            }
            catch (gnu.text.SyntaxException e) {
                showError(e, perr, "Syntax error!");
                return;
            }
            catch (java.io.IOException e) {
                showError(e, perr, "IO error!");
                return;
            }
            catch (Throwable e) {
                showError(e, perr);
                return;
            }
        }
        finally {
            if (out != null)
                ctx.consumer = saveConsumer;
        }
    }

    static void showError(Throwable exception, PrintWriter perr) {
        showError(exception, perr, "Error!");
    }

    static void showError(Throwable exception, PrintWriter perr, String dialogTitle) {
        KawaStackTraceView.logException(exception);
    }
}