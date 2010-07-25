require 'nodes/Assignment'
class RuleDefinitionStatement < Assignment
	def initialize(identifier,ruleList)
		super
	end
	def to_s
		"rules " + super
	end
end
