#!/usr/bin/env python

# example drawingarea.py

import pygtk
pygtk.require('2.0')
import gtk
import operator
import time
import string
import csv
import math
import random
import sys

class DrawingAreaExample:
    def __init__(self, file):
    	filereader = csv.reader(open(file, 'rb'), delimiter='\t')
    	self.points = set()
    	for row in filereader:
    		self.points.add( (int(row[0]), int(row[1])) )
    	
        window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        window.set_title("Clone Builder")
        window.connect("destroy", lambda w: gtk.main_quit())
        window.set_size_request(600, 600)
        self.area = gtk.DrawingArea()
        self.area.set_size_request(1000, 1000)
        self.pangolayout = self.area.create_pango_layout("")
        self.sw = gtk.ScrolledWindow()
        self.sw.add_with_viewport(self.area)
        self.table = gtk.Table(3,3)
        self.hruler = gtk.HRuler()
        self.vruler = gtk.VRuler()
        self.hruler.set_range(0, 1000, 0, 1000)
        self.vruler.set_range(0, 1000, 0, 1000)
        self.scale = gtk.Adjustment(lower=0, upper=30)
        self.slider = gtk.HScale(self.scale)
        self.button = gtk.Button("Output!")
        self.table.attach(self.hruler, 1, 2, 0, 1, yoptions=0)
        self.table.attach(self.vruler, 0, 1, 1, 2, xoptions=0)
        self.table.attach(self.sw, 1, 2, 1, 2)
        self.table.attach(self.slider, 1, 2, 2, 3, yoptions=0)
        self.table.attach(self.button, 2, 3, 2, 3, xoptions=0, yoptions=0)
        window.add(self.table)
        
        self.area.set_events(gtk.gdk.POINTER_MOTION_MASK |
                             gtk.gdk.POINTER_MOTION_HINT_MASK )
        self.area.connect("expose-event", self.area_expose_cb)
        def motion_notify(ruler, event):
            return ruler.emit("motion_notify_event", event)
        self.area.connect_object("motion_notify_event", motion_notify,
                                 self.hruler)
        self.area.connect_object("motion_notify_event", motion_notify,
                                 self.vruler)
        self.hadj = self.sw.get_hadjustment()
        self.vadj = self.sw.get_vadjustment()
        def val_cb(adj, ruler, horiz):
            if horiz:
                span = self.sw.get_allocation()[3]
            else:
                span = self.sw.get_allocation()[2]
            l,u,p,m = ruler.get_range()
            v = adj.value
            ruler.set_range(v, v+span, p, m)
            while gtk.events_pending():
                gtk.main_iteration()
        self.hadj.connect('value-changed', val_cb, self.hruler, True)
        self.vadj.connect('value-changed', val_cb, self.vruler, False)
        def size_allocate_cb(wid, allocation):
            x, y, w, h = allocation
            l,u,p,m = self.hruler.get_range()
            m = max(m, w)
            self.hruler.set_range(l, l+w, p, m)
            l,u,p,m = self.vruler.get_range()
            m = max(m, h)
            self.vruler.set_range(l, l+h, p, m)
        self.sw.connect('size-allocate', size_allocate_cb)
        def scale_change_cb(adj, event):
            return self.area.emit("expose-event", event)
        self.scale.connect("value_changed", scale_change_cb, None)
        self.button.connect("clicked", self.output_click_cb, None)
        
        self.area.show()
        self.hruler.show()
        self.vruler.show()
        self.sw.show()
        self.slider.show()
        self.button.show()
        self.table.show()
        window.show()

    def area_expose_cb(self, area, event):
        gc = gtk.gdk.GC(self.area.window)
        groups = self.group(self.scale.value)
        random.seed(0);
        for group in groups:
            r = random.randint(0, 65535)
            g = random.randint(0, 65535)
            b = random.randint(0, 65535)
            gc.set_rgb_fg_color(gtk.gdk.Color(r,g,b))
            for p in group:
	        self.area.window.draw_arc(gc, True, p[0], p[1], 7, 7, 0, 64*360)
        return True
    
    def output_click_cb(self, button, data=None):
        groups = self.group(self.scale.value)
        print "%d\t" % len(groups),
        # find lower threshold
        if len(groups) >= len(self.points):
            print "inf\t",
        else:
            u = self.scale.value; l = 0.0;
            while u-l > 0.1:
                n = (u + l) / 2.0
                ng = self.group(n)
                if len(ng) > len(groups):
                    l = n; continue
                else:
                    u = n; continue
            print "%f\t" % u,
        # upper threshold
        if len(groups) <= 1:
            print "inf\t",
        else:
            l = self.scale.value; u = self.scale.value*10.0;
            while u-l > 0.1:
                n = (u + l) / 2.0
                ng = self.group(n)
                if len(ng) > len(groups):
                    l = n; continue
                else:
                    u = n; continue
            print "%f\t" % l
        for group in groups:
            n = len(group)
            xmean = sum([x for (x,y) in group]) / float(n)
            ymean = sum([y for (x,y) in group]) / float(n)
            covxx = sum([(x-xmean)*(x-xmean) for (x,y) in group]) / float(n)
            covxy = sum([(x-xmean)*(y-ymean) for (x,y) in group]) / float(n)
            covyy = sum([(y-ymean)*(y-ymean) for (x,y) in group]) / float(n)
            xext = max([x for (x,y) in group]) - min([x for (x,y) in group])
            yext = max([y for (x,y) in group]) - min([y for (x,y) in group])
            print "\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f" % (len(group), xmean, ymean, covxx, covxy, covyy, xext, yext),
            for c in group:
                print "\t(%d,%d)" % (c[0],c[1]),
            print ""
            print ""
        #sys.exit(0)
    
    def group(self, threshold):
        remaining = self.points
        grouped = []
        current = set()
        while len(remaining) > 0:
            if len(current) == 0:
                current = set([setfirst(remaining)])
                remaining = remaining - current
            new = set()
            for r in remaining:
                for c in current:
                    if ((r[0]-c[0])*(r[0]-c[0]) + (r[1]-c[1])*(r[1]-c[1])) < threshold*threshold:
                        new.add(r)
            if len(new) > 0:
                current = current | new
                remaining = remaining - new
            else:
                grouped.append(current)
                current = set()
        return grouped
        

def setfirst(s):
    for e in s:
        return e

def main():
    gtk.main()
    return 0

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "need file to operate on!"
        sys.exit(-1)
    DrawingAreaExample(sys.argv[1])
    main()
