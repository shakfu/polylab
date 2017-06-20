#!/usr/bin/env python
import PyQt4
import sys
from PyQt4 import QtGui
from PyQt4 import QtCore

class HelloWindow(QtGui.QMainWindow):

	def __init__(self, win_parent = None):
		#Init the base class
		QtGui.QMainWindow.__init__(self, win_parent)
		self.create_widgets()


	def create_widgets(self):
		#Widgets
		self.label = QtGui.QLabel("Say hello:")
		self.hello_edit = QtGui.QLineEdit()
		self.hello_button = QtGui.QPushButton("Push Me!")

		#connect signal
		QtCore.QObject.connect(self.hello_button
			, QtCore.SIGNAL("clicked()")
			, self.on_hello_clicked)


		#Horizontal layout
		h_box = QtGui.QHBoxLayout()
		h_box.addWidget(self.label)
		h_box.addWidget(self.hello_edit)
		h_box.addWidget(self.hello_button)
		#Create central widget, add layout and set
		central_widget = QtGui.QWidget()
		central_widget.setLayout(h_box)
		self.setCentralWidget(central_widget)

	def on_hello_clicked(self):
		QtGui.QMessageBox.information(self
			, "Hello!"
			, "Hello %s" % self.hello_edit.displayText()
			, QtGui.QMessageBox.Ok)

if __name__ == "__main__":
	# Someone is launching this directly
	# Create the QApplication
	app = QtGui.QApplication(sys.argv)
	#The Main window
	main_window = HelloWindow()
	main_window.show()
	# Enter the main loop
	app.exec_()
