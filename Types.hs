module Types 
( Element
, ElementContent
) where


data Element = Element String [ElementContent] 
data ElementContent = Attribute String String | Subelement Element | Text String