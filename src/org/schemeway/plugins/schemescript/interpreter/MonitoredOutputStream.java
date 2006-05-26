/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.io.*;
import java.util.*;

import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;

class MonitoredOutputStream extends OutputStream implements IStreamMonitor {

    private List mListeners = new LinkedList();
	public static final String PROMPT = "> ";

    public MonitoredOutputStream() {
    }

    public void addListener(IStreamListener listener) {
        if (!mListeners.contains(listener))
            mListeners.add(listener);
    }

    public String getContents() {
    	return null;
        //return (mIsStdout ? MonitoredOutputStream.PROMPT : "");
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