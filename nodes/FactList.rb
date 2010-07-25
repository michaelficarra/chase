class FactList
	attr_reader :length
	def initialize(*facts)
		@facts = facts
		@length = facts.length
	end
	def to_s
		@length > 0 ? '{ '+@facts.collect{|f|f.to_s}.join(', ')+' }' : '∅'
	end
end
