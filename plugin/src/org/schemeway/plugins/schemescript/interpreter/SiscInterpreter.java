/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.interpreter;

import org.schemeway.plugins.schemescript.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SiscInterpreter extends AbstractInterpreter {
	public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".siscInterpreter";

	public SiscInterpreter() {
		super();
	}

	public IInterpreterProcess getProcess() {
		return SiscProcess.getInstance();
	}

	public String getConfigurationType() {
		return CONFIG_TYPE;
	}

	public void stop() {
	}

	public void restart() {
		start();
	}

	public boolean supportInterruption() {
		return false;
	}
}
