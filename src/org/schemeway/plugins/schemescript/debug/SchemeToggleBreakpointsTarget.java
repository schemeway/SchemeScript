/*
 * Copyright (c) 2005 SchemeWay.com
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.debug;

import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;
import org.eclipse.debug.ui.actions.*;
import org.eclipse.jdt.debug.core.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.editor.*;

/**
 * @author SchemeWay.com
 */
public class SchemeToggleBreakpointsTarget implements IToggleBreakpointsTarget {
    private static final String SCHEME_STRATUM = "Scheme";
    private SchemeEditor mEditor;
    
    public SchemeToggleBreakpointsTarget(SchemeEditor editor) {
        mEditor = editor;
    }
    
    protected SchemeEditor getEditor() {
        return mEditor;
    }
    
    public void toggleLineBreakpoints(IWorkbenchPart part, ISelection selection) throws CoreException {
        if (!(selection instanceof ITextSelection))
            return;
        
        ITextSelection textSelection = (ITextSelection) selection;
        
        IEditorInput input = getEditor().getEditorInput();
        if (!(input instanceof IFileEditorInput)) 
            return;
        
        IFileEditorInput fileInput = (IFileEditorInput) input;
        IFile resource = fileInput. getFile();
        
        IBreakpointManager manager = DebugPlugin.getDefault().getBreakpointManager();
        int lineno = textSelection.getStartLine() + 1;
        String sourceName = resource.getName();
        //String sourceName = null;
        
        String sourcePath = null; // Should be configurable
        //String sourcePath = resource.getRawLocation().toFile().getAbsolutePath();
        
        IBreakpoint breakpoint = null;
        
        if ((breakpoint = findBreakpoint(manager, resource, lineno)) != null) {
            manager.removeBreakpoint(breakpoint, true);
        }
        else {
            breakpoint = JDIDebugModel.createStratumBreakpoint(resource, SCHEME_STRATUM, sourceName, sourcePath, "*", lineno, -1, -1, 0, true, new HashMap());
            manager.addBreakpoint(breakpoint);
        }
    }
    
    private IBreakpoint findBreakpoint(IBreakpointManager manager, IResource resource, int lineno) {
        IBreakpoint[] breakpoints = manager.getBreakpoints();
        for(int i=0; i<breakpoints.length; i++) {
            IBreakpoint bp = breakpoints[i];
            if (bp.getMarker().getResource().equals(resource) &&
                    bp.getMarker().getAttribute(IMarker.LINE_NUMBER, -1) == lineno) 
                return bp;
        }
        return null;
    }

    public boolean canToggleLineBreakpoints(IWorkbenchPart part, ISelection selection) {
        return selection instanceof ITextSelection;
    }

    public void toggleMethodBreakpoints(IWorkbenchPart part, ISelection selection) throws CoreException {
    }

    public boolean canToggleMethodBreakpoints(IWorkbenchPart part, ISelection selection) {
        return false;
    }

    public void toggleWatchpoints(IWorkbenchPart part, ISelection selection) throws CoreException {
    }

    public boolean canToggleWatchpoints(IWorkbenchPart part, ISelection selection) {
        return false;
    }
}
