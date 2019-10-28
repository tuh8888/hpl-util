import os

from IPython.display import *
from ipywidgets import interactive, widgets


class FileViewer:

    def __init__(self, files, extension=None):

        if type(files) is str and extension is not None:
            self.extension = extension
            parent_dir = files if files.endswith('/') else files + '/'
            self.files = {f.replace('.' + f.split('.')[-1], ''): '%s%s' % (parent_dir, f) for f in
                          os.listdir(parent_dir) if f.endswith(self.extension)}

        if type(files) is list and files and type(files[0]) is str:
            self.extension = files[0].split('.')[-1]
            self.files = {f.replace('.' + f.split('.')[-1], ''): f for f in files if f.endswith(self.extension)}

        if self.extension is not None:
            viewers = {
                'pdf': self.pdf_viewer,
                'png': self.image_viewer,
                'md': self.markdown_viewer
            }
            self.viewer = viewers.get(self.extension)

    def show(self):
        return self.viewer()

    def display_image(self, file, scale):
        img = Image(self.files[file], height=512 * scale, width=512 * scale, unconfined=True)
        display(img)

    def display_markdown(self, file):
        md = Markdown(self.files[file])
        display(md)

    def display_pdf(self, file):
        pdf = HTML('<iframe src=%s width=700 height=350></iframe>' % self.files[file])
        display(pdf)

    def markdown_viewer(self):
        return interactive(self.display_markdown, file=list(self.files.keys()))

    def image_viewer(self):
        return interactive(self.display_image, file=list(self.files.keys()),
                           scale=widgets.FloatSlider(min=0.5, max=1.5, step=0.1, value=1))

    def pdf_viewer(self):
        return interactive(self.display_pdf, file=list(self.files.keys()))
