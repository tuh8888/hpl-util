from lxml import etree


class DrugBankParser:
    ns = "{http://www.drugbank.ca}"

    def __init__(self):
        drugbank = etree.parse("E:/Documents/Data/DrugBank/full database.xml")
        self.root = drugbank.getroot()

    def get_drugs(self):
        return self.root.findall(self.ns + "drug")

    """
    :argument entity: Drug or target
    """

    def get_name(self, entity):
        return entity.find(self.ns + "name").text

    def get_targets(self, drug):
        return drug.find(self.ns + "targets").findall(self.ns + "target")

    def get_id(self, entity):
        return entity.find(self.ns + "drugbank-id").text
