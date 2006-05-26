/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.io.*;
import java.net.*;
import java.util.*;

import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;
import org.schemeway.plugins.schemescript.*;

import sisc.*;
import sisc.env.*;
import sisc.interpreter.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SiscProcess implements IInterpreterProcess {

	private static class SiscStreamsProxy implements IStreamsProxy {

		private MonitoredOutputStream mErrorMonitor = new MonitoredOutputStream();
		private MonitoredOutputStream mOutputMonitor = new MonitoredOutputStream();
		private OutputStream mSiscInputStream;

		public SiscStreamsProxy() {
			try {
				PipedInputStream inputStream = new PipedInputStream();
				PipedOutputStream outputStream = new PipedOutputStream(inputStream);
				mSiscInputStream = outputStream;

				final AppContext appContext = new AppContext();
				Context.setDefaultAppContext(appContext);
				URL heapUrl = SchemeScriptPlugin.getDefault().find(new Path("/lib/sisc.shp"));
				appContext.addHeap(AppContext.openHeap(heapUrl));
				
				final DynamicEnvironment env = new DynamicEnvironment(appContext, inputStream,
						mOutputMonitor);
				Thread replRhread = new Thread() {
					public void run() {
						REPL repl = new REPL(env, REPL.getCliProc(appContext));
						repl.go();
					}
				};
				replRhread.run();
			}
			catch (IOException exception) {
				SchemeScriptPlugin.logException("unable to start sisc", exception);
			}
			catch (ClassNotFoundException exception) {
				SchemeScriptPlugin.logException("unable to start sisc", exception);
			}
		}

		public IStreamMonitor getErrorStreamMonitor() {
			return mErrorMonitor;
		}

		public IStreamMonitor getOutputStreamMonitor() {
			return mOutputMonitor;
		}

		public void write(final String input) throws IOException {
			mSiscInputStream.write(input.getBytes());
			mSiscInputStream.flush();
		}
	}

	private ILaunch mLaunch;
	private Map mAttributes = new HashMap();
	private SiscStreamsProxy mStreamsProxy;

	private static SiscProcess mInstance;
	
	private SiscProcess() {
		mStreamsProxy = new SiscStreamsProxy();
	}

	public boolean isRunning() {
		return getLaunch() != null;
	}

	public String getLabel() {
		return "Embedded SISC";
	}

	public ILaunch getLaunch() {
		return mLaunch;
	}

	public void setLaunch(ILaunch launch) {
		mLaunch = launch;
	}

	public IStreamsProxy getStreamsProxy() {
		return mStreamsProxy;
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

	public void sendToInterpreter(String code) {
		try {
			mStreamsProxy.write(code + "\n");
		}
		catch (IOException e) {
		}
	}

	public static SiscProcess getInstance() {
		if (mInstance == null) {
			mInstance = new SiscProcess();
		}
		return mInstance;
	}

}
