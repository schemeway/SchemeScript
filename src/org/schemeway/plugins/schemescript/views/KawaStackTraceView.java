/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.views;

import java.io.*;
import java.util.*;
import java.util.List;

import org.eclipse.core.resources.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.resource.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;

public class KawaStackTraceView extends ViewPart {

	private static class StackTraceContentProvider extends ArrayContentProvider implements
			ITreeContentProvider {

		private Throwable mInput;

		public Object[] getChildren(Object parentElement) {
			if (parentElement instanceof Throwable)
				return ((Throwable) parentElement).getStackTrace();
			return null;
		}

		public Object getParent(Object element) {
			if (element instanceof Throwable)
				return null;
			else
				return mInput;
		}

		public boolean hasChildren(Object element) {
			return (element instanceof Throwable);
		}
	}

	private static class StackTraceLabelProvider extends LabelProvider {
		public String getText(Object element) {
			if (element instanceof Throwable) {
				String message = ((Throwable) element).getMessage();
				if (message == null)
					message = element.getClass().toString();
				return message;
			}
			else {
				StackTraceElement stackElement = (StackTraceElement) element;

				String fileName = stackElement.getFileName();
				if (fileName != null) {
					File file = new File(fileName);
					return stackElement.getMethodName() + " - " + file.getName() + ":"
							+ stackElement.getLineNumber();
				}
				else {
					return stackElement.getMethodName() + " - ??";
				}
			}
		}

		public Image getImage(Object element) {
			if (element instanceof Throwable)
				return mStackElementImage;
			else {
				StackTraceElement stElement = (StackTraceElement) element;

				String fileName = stElement.getFileName();
				if (fileName != null && fileName.endsWith(".scm"))
					return mSchemeStackElementImage;
				else if (fileName != null && fileName.endsWith(".java"))
					return mJavaStackElementImage;
				else
					return mUnknownStackElementImage;
			}
		}
	}

	private class StackTraceDoubleClickListener implements IDoubleClickListener {
		public void doubleClick(DoubleClickEvent event) {
			IStructuredSelection selection = (IStructuredSelection) event.getSelection();
			if (selection.isEmpty())
				return;
			Object element = selection.getFirstElement();
			if (element instanceof StackTraceElement) {
				StackTraceElement stElement = (StackTraceElement) element;
				if (stElement.getLineNumber() >= 0) {
					IFile file = SchemeScriptTools.findFile(stElement.getFileName(), null);
					if (file != null) {
						SchemeScriptTools.openEditor(file, stElement.getLineNumber() - 1);
					}
				}
			}
		}
	}

	private class SchemeElementFilter extends ViewerFilter {

		public boolean select(Viewer viewer, Object parentElement, Object element) {
			if (element instanceof Throwable)
				return true;
			if (element instanceof StackTraceElement) {
				StackTraceElement stElement = (StackTraceElement) element;
				String filename = stElement.getFileName();
				return filename != null && filename.endsWith(".scm")
						&& stElement.getLineNumber() >= 0;
			}
			return false;
		}
	}

	private class SchemeElementFilterAction extends Action {
		public SchemeElementFilterAction() {
			super(null, IAction.AS_CHECK_BOX);
			setToolTipText("Show Scheme frames only");
			setImageDescriptor(mSchemeFilterDescriptor);
		}

		public void run() {
			if (mFiltered) {
				mStackList.removeFilter(mSchemeElementFilter);
				mFiltered = false;
				setChecked(false);
			}
			else {
				mStackList.addFilter(mSchemeElementFilter);
				mFiltered = true;
				setChecked(true);
			}
		}
	}

	private static final String VIEW_ID = SchemeScriptPlugin.PLUGIN_NS + ".kawa.stackTraceView";

	private static Image mStackElementImage = SchemeScriptImageRegistry
			.getImage("icons/stacktrace/stackElement.gif");
	private static Image mSchemeStackElementImage = SchemeScriptImageRegistry
			.getImage("icons/stacktrace/schemeStackElement.gif");
	private static Image mJavaStackElementImage = SchemeScriptImageRegistry
			.getImage("icons/stacktrace/javaStackElement.gif");
	private static Image mUnknownStackElementImage = SchemeScriptImageRegistry
			.getImage("icons/stacktrace/unknownStackElement.gif");
	private static ImageDescriptor mSchemeFilterDescriptor = SchemeScriptImageRegistry
			.getImageDescriptor("icons/stacktrace/schemeStackElement.gif");

	private TreeViewer mStackList;
	private ViewerFilter mSchemeElementFilter = new SchemeElementFilter();
	private boolean mFiltered = true;

	public KawaStackTraceView() {
		super();
	}

	public void createPartControl(Composite parent) {
		mStackList = new TreeViewer(parent, SWT.V_SCROLL);
		mStackList.setContentProvider(new StackTraceContentProvider());
		mStackList.setLabelProvider(new StackTraceLabelProvider());

		mStackList.addDoubleClickListener(new StackTraceDoubleClickListener());
		mStackList.addFilter(mSchemeElementFilter);
		mFiltered = true;

		IToolBarManager manager = this.getViewSite().getActionBars().getToolBarManager();

		SchemeElementFilterAction action = new SchemeElementFilterAction();
		action.setChecked(true);
		manager.add(action);
	}

	public void setFocus() {
		mStackList.getControl().setFocus();
	}

	private void setException(Throwable exception) {
		List throwables = new ArrayList();
		for (; exception != null; exception = exception.getCause())
			throwables.add(exception);

		mStackList.setInput(throwables.toArray(new Object[throwables.size()]));
	}

	public static void logException(Throwable exception) {
		logException(exception, exception.getMessage());
	}

	public static void logException(final Throwable exception, final String message) {
		// ensure we run in the UI thread - fix for Eclipse 3.1
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				if (!MessageDialog.openConfirm(shell, "Runtime Exception", message + "\nDebug?"))
					return;

				showStackTrace(exception);
			}
		});
	}

	public static void showStackTrace(Throwable exception) {
		try {
			KawaStackTraceView view = (KawaStackTraceView) (PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getActivePage().showView(VIEW_ID));
			view.setException(exception);
			view.setFocus();
		}
		catch (PartInitException e) {
		}
	}
}