require 'nodes/Assignment'
class FactDefinitionStatement < Assignment
	def initialize(identifier,factList)
		super
	end
	def to_s
		"facts " + super
	end
end
