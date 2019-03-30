from html.parser import HTMLParser
from urllib.request import urlopen

from ipywidgets import interactive


class HTMLSeparator(HTMLParser):
    def error(self, message):
        pass

    separated_data = {}
    current_section = None

    def __init__(self, website, tags):
        super(HTMLSeparator, self).__init__()
        self.tags = tags
        self.website = website

    def handle_starttag(self, tag, attrs):
        if tag in self.tags:
            for name, value in attrs:
                if name == "id":
                    self.current_section = value
                    self.separated_data[self.current_section] = ''

    def handle_data(self, data):
        if self.current_section is not None:
            self.separated_data[self.current_section] += data

    def display(self, section):
        print(self.separated_data[section])

    def show(self):
        with urlopen(self.website) as w:
            html = w.read().decode('utf-8')
            self.feed(html)

        return interactive(self.display, section=list(self.separated_data.keys()))
